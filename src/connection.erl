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

%% @since 2012-12-17
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc 
%% Connection module expose function for operations/queries on connected MySQL server. 
%% The module contains client API functions for all allowed operations.  
%%
%% @reference [http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol]

-module(connection).

%%
%% Include files
%%
-include("client_records.hrl").
-include("client_settings.hrl").
-include("mysql_types.hrl").

%%
%% Import modules
%%
-import(statement, []).
-import(conn, []).

%%
%% Exported Functions
%%
-export([
  is_open/1, 
  connection_record/1, 
  execute_query/2, 
  get_field_list/3, 
  prepare_statement/2, 
  get_prepared_statement_handle/2, 
  send_statement_long_parameter/4, 
  execute_statement/4, 
  execute_statement/6, 
  close_statement/2, 
  reset_statement/2, 
  fetch_statement/4, 
  transaction/2
]).

%%
%% API Functions
%%

%% @spec connection_record(Connection_pid::pid()) -> #connection{} | #mysql_error{}
%% 
%% @doc Returns #connection{} record decribed given connection. 
%%
connection_record(Connection_pid) ->
  gen_server_call(Connection_pid, connection_record).

%% @spec is_open(Connection_pid::pid()) -> true | false
%% 
%% @doc Returns true if connection is open and healthy. 
%%
is_open(Connection_pid) ->
  case gen_server_call(Connection_pid, is_open) of
    true -> true;
    _ -> false
  end.

%% @spec execute_query(Connection_pid::pid(), Query::string()) -> Result
%% Result = {Metadata::#metadata{}, ResultSet} | #mysql_error{}
%% ResultSet = list(Row)
%% Row = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%%
%% @doc Executes SQL query and return a tuple of field metadata information and list of result rows.
%% @see packet_response:handle_response/2
%%
execute_query(Connection_pid, Query) ->
  gen_server_call(Connection_pid, {execute_query, Query}).

%% @spec get_field_list(Connection_pid::pid(), Table_name::string(), Column_name::string()) -> Result 
%% Result = {Metadata::#metadata{}, []} | #mysql_error{}
%%
%% @doc Returns list of field metadata records for table 'Table_name'. Column_name is requalar 
%% expression using symbols % and _ to filter column names for return.
%%
get_field_list(Connection_pid, Table_name, Column_name) ->
  gen_server_call(Connection_pid, {get_field_list, Table_name, Column_name}).

%% @spec prepare_statement(Connection_pid::#connection{}, Statement::string()) -> Result
%% Result = {Metadata::#metadata{}, [#ok_stmt_packet{}]} | #mysql_error{}
%% 
%% @doc Send command to prepare statement. Returns #ok_stmt_packet.stmt_handle handle 
%% of prepared statement that will be used for followed operation with the statement.
%%
prepare_statement(Connection_pid, Statement) ->
  gen_server_call(Connection_pid, {prepare_statement, Statement}).

%% @spec get_prepared_statement_handle(Connection_pid::pid(), Statement::string()) -> integer()
%% 
%% @doc The same is prepare_statement/2 but it has short return: just a handle 
%% of a prepared statement.
%%
get_prepared_statement_handle(Connection_pid, Statement) ->
  case gen_server_call(Connection_pid, {prepare_statement, Statement}) of
    {_,[#ok_stmt_packet{stmt_handle = Handle}|_]} -> Handle;
    R -> R
  end.

%% @spec send_statement_long_parameter(Connection_pid, Handle, Parameter_index, Parameter) -> any()
%% Connection_pid = pid()
%% Handle = integer()
%% Parameter_index = integer()
%% Parameter = binary()
%% 
%% @doc Sends parameter of prepered statement to SQL server before statement execution.
%%
send_statement_long_parameter(Connection_pid, Handle, Parameter_index, Parameter) ->
  gen_server:cast(Connection_pid, {send_statement_long_parameter, Handle, Parameter_index, Parameter}).

%% @spec execute_statement(Connection_pid, Handle, Parameter_types, Parameters) -> Result
%% Connection_pid = pid()
%% Handle = integer()
%% Parameter_types = list(integer())
%% Parameters = list(term())
%% Result = {Metadata::#metadata{}, ResultSet} | #mysql_error{}
%% ResultSet = list(Row)
%% Row = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%%
%% @doc Executes prepared statement represented with Handler. Parameters will replace '?' placeholders in prepared statement. 
%% Parameter_types list passes types for each parameter. 
%% Returns result as a list of data rows.
%% @see execute_statement/5
execute_statement(Connection_pid, Handle, Parameter_types, Parameters) ->
  execute_statement(Connection_pid, Handle, Parameter_types, Parameters, ?CURSOR_TYPE_NO_CURSOR, true).

%% @spec execute_statement(Connection_pid, Handle, Parameter_types, Parameters, Flags, New) -> Result
%% Connection_pid = pid()
%% Handle = integer()
%% Parameter_types = list(integer())
%% Parameters = list(term())
%% Flags = integer()
%% New = boolean()
%% Result = {Metadata::#metadata{}, ResultSet} | #mysql_error{}
%% ResultSet = list(Row)
%% Row = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%% 
%% @equiv execute_statement/4
%% @doc The same as execute_statement/4 except parameter New. The New equals true forces command send parameter types to server.
execute_statement(Connection_pid, Handle, Parameter_types, Parameters, Flags, New) ->
  gen_server_call(Connection_pid, {execute_statement, Handle, Parameter_types, Parameters, Flags, New}).

%% @spec close_statement(Connection_pid::pid(), Handle::integer()) -> {Metadata::#metadata{}, []} | #mysql_error{}
%%
%% @doc The command closes prepared statement and frees corresponded resources on server.
%%
close_statement(Connection_pid, Handle) ->
  gen_server_call(Connection_pid, {close_statement, Handle}).

%% @spec reset_statement(Connection_pid::pid(), Handle::integer()) -> {Metadata::#metadata{}, []} | #mysql_error{}
%%
%% @doc Resets prepared statement and removes all cashed data on server.
%%
reset_statement(Connection_pid, Handle) ->
  gen_server_call(Connection_pid, {reset_statement, Handle}).

%% @spec fetch_statement(Connection_pid, Handle, Metadata, Row_number) -> Result
%% Connection_pid = pid()
%% Handle = integer()
%% Metadata = #metadata{}
%% Row_number = integer()
%% Result = {Metadata::#metadata{}, ResultSet} | #mysql_error{}
%% ResultSet = list(Row)
%% Row = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%%
%% @doc Fetchs data from a cursor that was created by statement execution.
%%
fetch_statement(Connection_pid, Handle, Metadata, Row_number) ->
  gen_server_call(Connection_pid, {fetch_statement, Handle, Metadata, Row_number}).

%% @spec transaction(Connection_pid::pid(), F::fun()) -> any() | #mysql_error{type=transaction}
%% 
%% @doc executes a transaction. F represents a function with sequence of queries under the transaction management.
%% (f.e F = fun(Connection) -> my:execute_query(...), ... end). The function returns result of F/1 if transaction is successful
%% and returns #mysql_error{} if transaction was rollbacked.
%%
transaction(Connection_pid, F) ->
  R = try 
    case connection:execute_query(Connection_pid, "START TRANSACTION") of
      {_,[#ok_packet{}]} -> F(Connection_pid);
      Msql_err -> Msql_err
    end
  catch
    _:Err -> #mysql_error{type = transaction, source = "connection:transaction/2", message = Err}
  end,
%  io:format(">>> transaction ~p~n", [R]),
  case R of
    #mysql_error{} -> 
      case connection:execute_query(Connection_pid, "ROLLBACK") of
        {_,[#ok_packet{}]} -> R#mysql_error{type = transaction};
        RollBackErr -> RollBackErr
      end;
    _ ->
      case connection:execute_query(Connection_pid, "COMMIT") of
        {_,[#ok_packet{}]} -> R;
        CommitErr -> CommitErr
      end
  end.

%%
%% Private functions
%%

gen_server_call(Conn_pid, Request) ->
  case is_process_alive(Conn_pid) of
    true ->  
      try 
        gen_server:call(Conn_pid, Request, ?GEN_SERVER_TIMEOUT)
      catch
        exit:Reason -> #mysql_error{type = connection, 
              source = lists:concat(["connection:gen_server_call(", pid_to_list(Conn_pid), ", ", req_for_concat(Request), "): exit is catched."]),
              message = Reason}
      end;
    false -> #mysql_error{type = connection, 
              source = lists:concat(["connection:gen_server_call(", pid_to_list(Conn_pid), ", ", req_for_concat(Request), ")."]),
              message = lists:concat(["process ", pid_to_list(Conn_pid), " is not alive."])}
  end.

req_for_concat(Request) when is_atom(Request) -> Request;
req_for_concat(Request) when is_list(Request) -> Request;
req_for_concat(Request) when is_tuple(Request) -> element(1, Request).