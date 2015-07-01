%%
%% Copyright (C) 2010-2014 by krasnop@bellsouth.net (Alexei Krasnopolski)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License. 
%%

%% @since 2011-08-08
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc 
%% The module implements gen_server behaviour for connection process. The module provides callback functions
%% for all operations with MySQL server connection.
%%
%% @reference [http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol]
%% 

-module(conn_srv).
-behaviour(gen_server).

%%
%% Include files
%%
-include("client_settings.hrl").
-include("client_records.hrl").
-include("mysql_commands.hrl").
-include("mysql_types.hrl").

%%
%% Import modules
%%
-import(io_send_cmd, []).
-import(helper_statement, []).

%%
%% Exported API Functions
%%
-export([
  create_connection/1
]).

%%
%% Internal exports. (gen_server callbacks)
%%
-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2,
  terminate/2, 
  code_change/3
]).

%%
%% API Functions
%%

%% @spec create_connection(Args::#datasource{}) -> pid() 
%% 
%% @doc Opens ip/tcp socket connection to remote MySQL server. The function returns a pid 
%% of the connection process.
%% 
create_connection(Arg) ->
%  io:format(user, " >>> conn_srv:create_connection/1~n", []),
  case R = gen_server:start(?MODULE, Arg, []) of
%% Set transaction isolation level:
    {ok, Conn} ->
      case Connection = connection:connection_record(Conn) of
        #connection{} ->
          case C = gen_server:call(Conn, set_trans_level, ?GEN_SERVER_TIMEOUT) of
            #mysql_error{} ->
              io:format(user, "can not set transaction isolation level.~n", []), %% TODO : how transit the error to client?
              {error, C};
              _            -> R
          end;
        #mysql_error{} -> gen_server:cast(Conn, stop),
          {error, Connection}
      end;
    {error, _} -> R
  end.

%%
%% Generic server callback functions.
%%

%% @spec init(DS_def::#datasource{}) -> Return
%% Return = {ok, State::term()} | {stop, Error::#mysql_error{}}
%% @private
%% @doc Initiates the server.
%%
init(DS_def) ->
%  io:format(user, " >>> conn_srv:init/1 ds_def: ~p~n", [DS_def]),
  {ok, helper_connection:open_connection(DS_def)}.

%% @spec handle_call(Request::term(), From::pid(), State::term()) -> Return 
%% Return = {reply, Reply, State::term()}          |
%%          {reply, Reply, State::term(), Timeout} |
%%          {noreply, State::term()}               |
%%          {noreply, State::term(), Timeout}      |
%%          {stop, Reason, Reply, State::term()}   |
%%          {stop, Reason, State::term()}
%% @private 
%% @doc Handling call messages.
%%
handle_call(connection_record, _From, Connection) ->
  {reply, Connection, Connection};

handle_call(is_open, _From, Connection) ->
  case io_send_cmd:send_cmd_packet(Connection, <<?PING>>) of
    {#metadata{},[#ok_packet{}]} -> {reply, true, Connection};
    _                            -> {reply, false, Connection}
  end;

handle_call({change_user, User, Password, Db}, _From, Connection) ->
  New_connection = helper_connection:change_user(Connection, User, Password, Db),
  {reply, ok, New_connection};

handle_call({change_owner, Owner}, _, Connection) ->
  {reply, ok, Connection#connection{owner = Owner}};

handle_call({execute_query, Query}, {Caller, _}, #connection{owner = Owner} = Connection) ->
  case Caller =:= Owner of
    true -> 
      Value = io_send_cmd:send_cmd_packet(Connection, <<?EXEC_QUERY, (list_to_binary(Query))/binary>>),
      {reply, Value, Connection};
    false -> {reply, #mysql_error{type = pooling, 
                                  source = "conn_srv:handle_call({execute_query, _}, _, _)", 
                                  message = "not owner: connection has returned to pool."}, Connection}
  end;

handle_call(set_trans_level, _From, Connection) ->
  Query = "SET SESSION TRANSACTION ISOLATION LEVEL " ++ 
    case ((Connection#connection.ds_def)#datasource.flags)#client_options.trans_isolation_level of
      read_uncommitted -> "READ UNCOMMITTED";
      read_committed -> "READ COMMITTED";
      serializable -> "SERIALIZABLE";
      repeatable_read -> "REPEATABLE READ";
      _ -> "REPEATABLE READ"
    end,
  Value = io_send_cmd:send_cmd_packet(Connection, <<?EXEC_QUERY, (list_to_binary(Query))/binary>>),
  {reply, Value, Connection};

handle_call({select_db, DBName}, _From, Connection) ->
  Value = io_send_cmd:send_cmd_packet(Connection, <<?SELECT_DB, (list_to_binary(DBName))/binary>>),
  conn_pool_srv:remove(Connection),
  {reply, Value, (Connection#connection.ds_def)#datasource{database = DBName}};

handle_call({get_field_list, Table_name, Column_name}, _From, Connection) ->
  Value = io_send_cmd:send_cmd_packet(Connection, <<?GET_FIELD_LIST, (list_to_binary(Table_name))/binary, 0:8, (list_to_binary(Column_name))/binary>>),
  {reply, Value, Connection};

handle_call({prepare_statement, Statement}, _From, Connection) ->
  Value = io_send_cmd:send_cmd_packet(Connection, <<?PREPARE_STATEMENT, (list_to_binary(Statement))/binary>>, {0, prepare}),
  {reply, Value, Connection};

handle_call({execute_statement, Handle, Parameter_types, Parameters, Flags, New}, _From, Connection) ->
  Par_types_binary = if 
    New -> <<1:8, (helper_statement:pack_types(Parameter_types))/binary>>; 
    true -> <<0:8>> 
  end,
  NULL_bit_map = helper_statement:pack_null_bit_map(Parameter_types, Parameters),
  IsCursor = if Flags > ?CURSOR_TYPE_NO_CURSOR -> cursor; true -> none end,
  Value =
    case Return = helper_statement:pack_parameters(Parameter_types, Parameters) of
      #mysql_error{} -> Return;
      Par_binary ->  io_send_cmd:send_cmd_packet(Connection, <<?EXECUTE_STATEMENT, Handle:32/little-integer, 
              Flags:8/integer, 1:32/little-integer, NULL_bit_map/binary, 
              Par_types_binary/binary, Par_binary/binary>>, {0, IsCursor})
    end,
  {reply, Value, Connection};

handle_call({close_statement, Handle}, _From, Connection) ->
  Value = io_send_cmd:send_cmd_packet_no_resp(Connection, <<?CLOSE_STATEMENT, Handle:32/little-integer>>),
  {reply, Value, Connection};

handle_call({reset_statement, Handle}, _From, Connection) ->
  Value = io_send_cmd:send_cmd_packet(Connection, <<?RESET_STATEMENT, Handle:32/little-integer>>, 0),
  {reply, Value, Connection};

handle_call({fetch_statement, Handle, Metadata, Row_number}, _From, Connection) ->
  Value = io_send_cmd:send_cmd_packet(Connection, <<?FETCH_STATEMENT, Handle:32/little-integer, Row_number:32/little-integer>>, Metadata, {0, fetch}),
  {reply, Value, Connection}.

%% @spec handle_cast(Request::term(), State::term()) -> Return
%% Return = {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%% @private
%% @doc Handling cast messages.
%%
handle_cast({send_statement_long_parameter, Handle, Parameter_index, Parameter}, Connection) ->
  io_send_cmd:send_cmd_packet_no_resp(Connection, <<?SEND_STATEMENT_LONG_PARAMETER, Handle:32/little-integer, 
    Parameter_index:16/little-integer, Parameter/binary>>),
  {noreply, Connection};

handle_cast(break_connection, Connection) ->
  io:format(user, " >>> conn_srv:handle_cast(break_connection, ..)~n", []),
%%  io_send_cmd:send_cmd_packet_no_resp(Connection, <<?CLOSE>>),
  gen_tcp:close(Connection#connection.socket),
  {noreply, Connection};

handle_cast(close_connection, Connection) ->
  io_send_cmd:send_cmd_packet_no_resp(Connection, <<?CLOSE>>),
  gen_tcp:close(Connection#connection.socket),
  {stop, normal, Connection};

handle_cast(stop, Connection) ->
  {stop, normal, Connection}.

%% @spec handle_info(Info::term(), State::term()) -> Return
%% Return = {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}
%% @private
%% @doc Handling all non call/cast messages.
%%
handle_info(timeout, Connection) -> 
  io:format(user, " ### handle_info timeout~n", []),
  {stop, normal, Connection};

handle_info(Info, Connection) -> 
  case Info of
    {tcp, _Port, Bin} ->
      <<Output:100/bytes, _B1/binary>> = Bin,
      io:format(user, " ### handle_info: ~p receives unexpected binary ~p~n", [self(), Output]);
    {tcp_closed, Port} ->
      io:format(user, " ### handle_info: socket ~p is closed, pid: ~p~n", [Port, self()]);
    _ ->
      io:format(user, " ### handle_info: ~p gets ~p~n", [self(), Info])
  end,
  io:format(user, " ### handle_info: Connection: ~p~n", [Connection]),
  {stop, normal, Connection}.

%% @spec terminate(Reason::term(), State::term()) -> any() 
%% @private
%% @doc Shutdown the server and returns any (ignored by gen_server).
%%
terminate(_Reason, _State) -> 
%%  io:format(user, " ### terminate ~p ~p~n", [_Reason, self()]),
%%  io:format(user, " ### terminate ~p<end reason>;~n~p<end state>;~n~p<end self>;~n", [_Reason, _State, self()]),
  ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, State}
%% @private
%% @doc Converts process state when code is changed.
%%
code_change(_OldVsn, State, _Extra) -> {ok, State}.
