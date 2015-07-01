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

%% @since 2012-12-18
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc 
%% Datasource module implements API to manage datasource process. The process keeps information
%% about given datasource and controlls a pool of connections to DS. A functions get_connection/1,
%% return_connection/2 and invalidate_connection/2 directly delegate calls to the connection pool that creates/reuses/disposes
%% a connections. We can think about datasource module as a wrapper for resource_pool_srv module.   
%%
%% @headerfile "client_records.hrl"

-module(datasource).

%%
%% Include files
%%
-include("client_settings.hrl").
-include("client_records.hrl").

%%
%% Import modules
%%
-import(resource_pool_srv, []).

%%
%% Exported Functions
%%
-export([
  close/1,
  change_user/4, 
  select_db/2,
  get_connection/1,
  return_connection/2,
  invalidate_connection/2
]).

%%
%% API Functions
%%

%% @spec close(DS_name::atom()) -> ok | {error,Error}
%%
%% @doc Stops datasource DS_name and disposes all linked resources.
%%
close(DS_name) ->
%  io:format(user, " >>> datasource:close_datasource/1: ~p ~n", [DS_name]),
%  io:format(user, "     children list:~p~n", [supervisor:which_children(DS_name)]),
  resource_pool:close(DS_name),
  supervisor:terminate_child(datasource_sup, DS_name),
  supervisor:delete_child(datasource_sup, DS_name).

%% @spec change_user(Connection_name::pid(), User::string(), Password::string(), Db::string) -> #connection{} | #mysql_error{}
%%
%% @doc Changes user for the datasource. This change concerns all
%% connections of the connected datasource/pool.
%% @see helper_connection:change_user/4
change_user(DS_name, User, Password, Db) -> 
  Connections = gen_server:call(DS_name, get_all_resources, ?GEN_SERVER_TIMEOUT),
  lists:foreach(fun (C) -> gen_server:call(C, {change_user, User, Password, Db}, ?GEN_SERVER_TIMEOUT) end, Connections).

%% @spec select_db(Connection_name::pid(), DBName::string()) -> Result
%% Result = {Metadata::#metadata{}, [#ok_packet{}]} | #mysql_error{}
%%
%% @doc Selects or changes current database (or schema). Change concerns all
%% connections of the given datasource/pool.
%%
select_db(DS_name, DBName) ->
  Connections = gen_server:call(DS_name, get_all_resources, ?GEN_SERVER_TIMEOUT),
  lists:foreach(fun (C) -> gen_server:call(C, {select_db, DBName}, ?GEN_SERVER_TIMEOUT) end, Connections).

%% @spec get_connection(DS_name::atom()) -> pid() | #mysql_error{}
%% 
%% @doc Provides with connection process pid. Connection pool returns idle connection or creates new
%% connection process if no idle connection is found.
%%
get_connection(DS_name) ->
%  io:format(user, " >>> datasource:get_connection(~p)~n", [DS_name]),
  resource_pool:borrow(DS_name).

%% @spec return_connection(DS_name::atom(), Resource::pid()) -> ok | #mysql_error{}
%% 
%% @doc Returns the connection to connection pool.
%%
return_connection(DS_name, Resource) ->
  resource_pool:return(DS_name, Resource).

%% @spec invalidate_connection(DS_name::atom(), Resource::pid()) -> ok | #mysql_error{}
%% 
%% @doc Sends request to dispose the connection.
%%
invalidate_connection(DS_name, Resource) ->
  resource_pool:invalidate(DS_name, Resource).
