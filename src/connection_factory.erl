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

%% @since 2012-12-11
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc
%% The module is a factory of resources - factory of mysql connections. 
%%

-module(connection_factory).
-behaviour(resource_factory).

%% ====================================================================
%% behaviour functions
%% ====================================================================
-export([create/1, destroy/2, validate/2, activate/2, passivate/2]).

%% @spec create(Resource_metadata::term()) -> pid()
%% 
%% @doc Creates new connection to MySQL server.
create(Resource_metadata) ->
  conn_srv:create_connection(Resource_metadata).

%% @spec destroy(Resource_metadata::term(), _Resource::pid()) -> noreturn()
%% 
%% @doc Destroyes resource - connection to server. 
destroy(_Resource_metadata, Resource) ->
  case is_process_alive(Resource) of
    true -> gen_server:cast(Resource, close_connection);
    false -> void
  end.

%% @spec validate(Resource_metadata::term(), _Resource::pid()) -> boolean()
%% 
%% @doc Validate connection: if connection is alive returns true, otherwise - false.
validate(_Resource_metadata, Resource) ->
  connection:is_open(Resource).

%% @spec activate(Resource_metadata::term(), _Resource::pid()) -> ok
%% 
%% @doc Some action during activation of connection before moving resource from pool to client.
activate({_Resource_metadata, Owner}, Resource) ->
  case is_process_alive(Resource) of
    true -> gen_server:call(Resource, {change_owner, Owner});
    false -> void
  end.

%% @spec passivate(Resource_metadata::term(), _Resource::pid()) -> ok
%% 
%% @doc Some action during passivation of connection after returning resource from use to pool.
passivate(_Resource_metadata, Resource) ->
  case is_process_alive(Resource) of
    true -> gen_server:call(Resource, {change_owner, undefined});
    false -> void
  end.

