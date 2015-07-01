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

%% @since 2010-11-05
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc 
%% This is a main module of Erlang MySQL client. The module implements application behaviour of the client and
%% is a supervisor for set of datasource.
%%
%% @reference [http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol]
%% @headerfile "client_records.hrl"

-module(my).
-behaviour(application).
-behaviour(supervisor).

%%
%% Include files
%%
-include("client_records.hrl").

%%
%% Import modules
%%

%%
%% Exported Functions
%%
-export([
  start_client/0,
  stop_client/0,
  new_datasource/1,
  new_datasource/2,
  new_datasource/3
]).

%%
%% Internal exports. (application behaviour callbacks)
%%
-export([
  start/2,
  prep_stop/1,
  stop/1
]).

%%
%% Internal exports. (gen_supervisor behaviour callbacks)
%%
-export([
  init/1
]).

%%
%% Application callback functions
%%

%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
%% @spec start(Type::term(), Args::list()) -> Return
%% Return = {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% 
%% @doc Starts the root supervisor of the client.
%% @private
start(_Type, []) ->
% io:format(user, " >>> my_app:start/2~n", []),
  supervisor:start_link({local, datasource_sup}, ?MODULE, []).

%% @spec stop(State::term()) -> any() 
%% 
%% @doc This is callback after application shut down.
%% @private
stop(_State) ->
% io:format(user, " <<< my_app:stop/1 ~p~n", [_State]),
  ok.

%% @spec prep_stop(A::term()) -> any() 
%% 
%% @doc this is callback before application is starting shut down. It closes all
%% datasources of application.
%% @private
prep_stop(_A) ->
%  io:format(user, " <<< my_app:prep_stop/1 ~p~n", [_A]),
%  io:format(user, "     children list:~p~n", [supervisor:which_children(datasource_sup)]),
  F = fun({Name, _, _, _}) -> datasource:close(Name) end,
  lists:foreach(F, supervisor:which_children(datasource_sup)).

%%
%% Supervisor callback functions
%%

%% @spec init(Arg::term()) -> {ok, {SupFlags, []}}
%%
%% @doc Initiates the supervisor.
%% @private
init(_) ->
  {ok,
    { {one_for_one, 0, 1}, %% Restart strategy
      []
    }
  }.

%%
%% API Functions
%%

%% @spec start_client() -> ok | {error, Reason}
%% 
%% @doc The function loads and starts OTP application named 'mysql_client'
%%
start_client() ->
  application:load(
    {application, 
     mysql_client,
     [
       {description, "MySQL native client"},
       {vsn, "1.2.8"},
       {modules, 
         [
           my,
           datasource,
           connection_factory,
           connection,
           conn_srv,
           packet_parser_binary,
           packet_parser,
           packet_response,
           packet_parser_string,
           io_socket,
           io_send_cmd,
           helper_statement,
           helper_connection,
           helper_common
         ]
       },
       {registered, [datasource_sup]},
       {applications, [kernel, stdlib]},
       {mod, {my, []}}
     ]
    }),
  application:start(mysql_client).

%% @spec stop_client() -> ok | {error, Reason}
%%
%% @doc The function stops and unload 'mysql_client' application.
%%
stop_client() ->
  application:stop(mysql_client),
  application:unload(mysql_client).

%% @spec new_datasource(DS_name::atom(), DS_def::#datasource{}, Pool_options::list(tuple())) -> Result
%% Result = {ok, Pid} | #mysql_error{}
%% 
%% @doc the function starts new child process registered as DS_name and implements
%% datasorce (or connection pool) functionality. The datasource process is controlled by
%% supervisor 'datasource_sup'.
%%
new_datasource(DS_name, DS_def, Pool_options) ->
%  io:format(user, " >>> my:new_datasource/3: ~p ~p ~p~n", [DS_name, DS_def, Pool_options]),
  Child_spec = {
    DS_name,
    {resource_pool, new,
      [DS_name, connection_factory, DS_def#datasource{name = DS_name}, Pool_options]
    },
    transient,
    brutal_kill,
    worker,
    [resource_pool_srv]
  },
  R = supervisor:start_child(datasource_sup, Child_spec),
%% >>> debug
%%  io:format(user, "     returns: ~p~n", [R]),
%%  io:format(user, "     from name to pid: ~p~n", [whereis(DS_name)]),
%% <<< debug
  case R of
    {ok, _Pid} -> R;
    {error, Reason} -> #mysql_error{type = datasource, 
                                    source = "my:new_datasource/3",
                                    message = Reason}
  end.

%% @spec new_datasource(DS_name::atom(), DS_def::#datasource{}) -> Result
%% Result = {ok, Pid} | #mysql_error{}
%% 
%% @doc the function starts new child process registered as DS_name and implements
%% datasorce (or connection pool) functionality. The datasource process is controlled by
%% supervisor 'datasource_sup'.
%%
new_datasource(DS_name, DS_def) -> new_datasource(DS_name, DS_def, []).

%% @spec new_datasource(DS_def::#datasource{}) -> Result
%% Result = {ok, Pid} | #mysql_error{}
%% 
%% @doc the function starts new child process and implements
%% datasorce (or connection pool) functionality. The datasource process is controlled by
%% supervisor 'datasource_sup'.
%%
new_datasource(DS_def) -> new_datasource(DS_def#datasource.name, DS_def).
