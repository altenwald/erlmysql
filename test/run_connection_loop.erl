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

%% @hidden
%% @since 2011-05-23
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc This module is running unit tests for concurrent connections.

-module(run_connection_loop).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("client_records.hrl").
-include("mysql_types.hrl").
-include("test.hrl").

%%
%% Import modules
%%

%%
%% Exported Functions
%%
-export([
  run_consumer/0
]).

%%
%% API Functions
%%

%% run_test_() ->
%% 	[ 
%%     { foreachx, 
%%       fun testing:do_setup/1, 
%%       fun testing:do_cleanup/2,
%%       [ 
%% 		    {{plain, run_consumer}, fun run_consumer/2}
%%       ]
%%     }
%% 	].

%%
%% Local Functions
%%

%% @spec run_consumer(X, C) -> any()
%% @doc The function establishes connection to database and recursively process messages
%% from database.
%%
run_consumer() -> 
	testing:do_start(),
  my:new_datasource( serializable_ds,
    #datasource{
		  name = serializable_ds,
		  host = ?TEST_SERVER_HOST_NAME, 
		  port = ?TEST_SERVER_PORT, 
		  database = "eunitdb", 
		  user = "user", 
		  password = "user",
		  flags = #client_options{trans_isolation_level = serializable}
	  }, 
    [{max_active, 210}, {test_on_borrow, true}]
  ),
  Conn = datasource:get_connection(serializable_ds),
	connection:execute_query(Conn, "UPDATE all_in_one SET int_col = 0 WHERE persist_id < 11"), 
%% 	List_ids = [{1,1},{2,2},{3,1},{4,1},{5,1},{6,3},{7,1},{8,4},{9,5},{10,1},
%% 		{11,7},{12,1},{13,8},{14,9},{15,10},{16,4},{17,4},{18,9},{19,9},{20,10}],
  List_ids = [{I, random:uniform(10)} || I <- lists:seq(1, 210)],
	Self = self(),
	lists:map(fun({Worker_id, Id}) -> spawn(fun() -> process_entry(Self, Id, Worker_id) end) end, List_ids),
	
	wait_all(20 * 180),
	
	{_,[[Val]|_]} = connection:execute_query(Conn, "SELECT int_col FROM all_in_one WHERE persist_id=1"),
  datasource:return_connection(serializable_ds, Conn),
  datasource:close(serializable_ds),
	?assertEqual(7, Val),
	?debug_Fmt("Val=~p",[Val]),
  testing:do_stop(ok).

wait_all(0) -> ok;
wait_all(N) ->
	receive
		done -> wait_all(N - 1)
	end.

%% @spec process_entry(Connection::#connection{}, Id::integer()) -> any()
%% @doc Marks message with Id as processed.
%%
process_entry(Self, Id, Worker_id) ->
	Na = resource_pool:get_num_active(serializable_ds),
  Ni = resource_pool:get_num_idle(serializable_ds),
  ?debug_Fmt(" >>> Process message [id=~p] worker #~p, pid ~p, pool{~p:~p}", [Id, Worker_id, self(), Na, Ni]),
  Conn = case datasource:get_connection(serializable_ds) of 
    {error, Err} ->
      ?debug_Fmt(" ### Cannot get connection for pid ~p with reason: ~p", [self(), Err]),
      process_entry(Self, Id, Worker_id);
   C -> C
  end,
	try
%	connection:execute_query(Conn,"SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE"),
    H1 = connection:get_prepared_statement_handle(Conn,
		  	"SELECT int_col FROM all_in_one WHERE persist_id=?"),
	  H2 = connection:get_prepared_statement_handle(Conn,
		  	"UPDATE all_in_one SET int_col=? WHERE persist_id=?"),
	  run_transaction(Conn, H1, H2, Id, Worker_id),
	  connection:close_statement(Conn, H1),
	  connection:close_statement(Conn, H2),
	  datasource:return_connection(serializable_ds, Conn)
  catch
    error:Reason ->  ?debug_Fmt(" ### Error in process  ~p with reason: ~p~nStack:~n~p", [self(), Reason, erlang:get_stacktrace()]),
                     datasource:return_connection(serializable_ds, Conn)
  end,
%%  timer:sleep(1000),
  Self ! done,
  process_entry(Self, Id, Worker_id).

run_transaction(Conn, H1, H2, Id, Worker_id) ->
	case TR = connection:transaction(Conn, fun(C) ->
			case QR = connection:execute_statement(C, H1, [?LONGLONG], [Id]) of
        #mysql_error{} -> QR; 
        {_,[[Val] | _]} ->
          RR = connection:execute_statement(C, H2, [?LONG, ?LONGLONG], [Val + 1, Id]),
          timer:sleep(150),
          RR
      end
		end) 
	of
		#mysql_error{type = transaction, errno = 1213, sqlstate = _, source = _, message = _M} ->
				?debug_Fmt(" *** restarting transaction *** [id=~p] worker #~p; ~p", [Id, Worker_id, _M]),	
				timer:sleep(300),
				run_transaction(Conn, H1, H2, Id, Worker_id);
    #mysql_error{} -> TR;
		{#metadata{}, [#ok_packet{}]} -> ?debug_Fmt(" <<< End process message [id=~p] worker #~p;", [Id, Worker_id]) 
	end.