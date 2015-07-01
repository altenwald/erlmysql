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
%% @since 2011-05-08
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc This module is running unit tests for statements command of MySQL protocol.

-module(cursor_tests).

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
-import(helper_statement, []).

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

cursor_test_() ->
	[
%%  DEBUG   
%%    { setup, fun testing:do_start/0, fun testing:do_stop/1, [
%%  GUBED  
    { foreachx, 
      fun do_setup/1, 
      fun testing:do_cleanup/2, 
      [
        {{plain, cursor_new},     fun cursor_new/2},
        {{plain, cursor_error},   fun cursor_error/2},
        {{plain, cursor_next},    fun cursor_next/2},
        {{plain, cursor_get},     fun cursor_get/2},
        {{plain, cursor_size},    fun cursor_size/2},
        {{plain, cursor_set},     fun cursor_set/2},
        {{plain, cursor_reset},   fun cursor_reset/2},
        {{plain, cursor_skip},    fun cursor_skip/2},
        {{plain, cursor_back},    fun cursor_back/2},
        {{plain, cursor_foreach}, fun cursor_foreach/2}
      ]
    }
%%  DEBUG   
%%      ]}
%%  GUBED  
	].

do_setup(X) ->
  Pid = testing:do_setup(X),
  Connection = datasource:get_connection(Pid),
  Result = connection:execute_query(Connection, "SELECT * FROM all_in_one WHERE persist_id > 10 LIMIT 27"),
  datasource:return_connection(Pid, Connection),
  Result.

%%
%% Local Functions
%%

cursor_next(_X, Result) -> fun() ->
  {M,[R|_] = RS} = Result,
	?debug_Fmt("~p  L=~p ~n", [R, length(RS)]),
	?assert(is_record(M, metadata)),
	?assertEqual(30, M#metadata.field_count),

  Cursor = cursor:new(Result),
  ?assertEqual(11, cursor:get(Cursor, "persist_id")),
  ?assertEqual(547732, cursor:get(Cursor, "int_col")),

  ?assertEqual(11, cursor:get(Cursor, "persist_id")),
  ?assertEqual(547732, cursor:get(Cursor, "int_col")),
  cursor:next(Cursor),
  ?assertEqual(12, cursor:get(Cursor, "persist_id")),
  ?assertEqual("Varchar value", cursor:get(Cursor, "varchar_col")),

  ?PASSED
end.

cursor_get(_X, Result) -> fun() ->
  Cursor = cursor:new(Result),
  cursor:next(Cursor),
  ?assertEqual(12, cursor:get(Cursor, "persist_id")),
  ?assertEqual("Varchar value", cursor:get(Cursor, "varchar_col")),
  ?assertEqual(null, cursor:get(Cursor, "decimal_34_14_col")),
  ?assertEqual(12, cursor:get(Cursor, 1)),
  ?assertEqual("Varchar value", cursor:get(Cursor, 3)),
  ?assertEqual(null, cursor:get(Cursor, 4)),
  
  ?PASSED
end.

cursor_size(_X, Result) -> fun() ->
  Cursor = cursor:new(Result),
  ?assertEqual(27, cursor:size(Cursor)),
  ?PASSED
end.

cursor_skip(_X, Result) -> fun() ->
  Cursor = cursor:new(Result),
  cursor:skip(Cursor, 4),
  ?assertEqual({mysql_time,false,1983,3,25,0,0,0,0}, cursor:get(Cursor, 6)),
  ?PASSED
end.

cursor_back(_X, Result) -> fun() ->
  Cursor = cursor:new(Result),
  cursor:set(Cursor, 7),
  cursor:back(Cursor),
  cursor:back(Cursor),
  ?assertEqual({mysql_time,false,1983,3,25,0,0,0,0}, cursor:get(Cursor, 6)),
  ?PASSED
end.

cursor_set(_X, Result) -> fun() ->
  Cursor = cursor:new(Result),
  cursor:set(Cursor, 7),
  ?assertEqual({mysql_time,false,1980,12,25,23,55,0,0}, cursor:get(Cursor, 8)),
  ?PASSED
end.

cursor_reset(_X, Result) -> fun() ->
  Cursor = cursor:new(Result),
  cursor:set(Cursor, 7),
  ?assertEqual({mysql_time,false,1980,12,25,23,55,0,0}, cursor:get(Cursor, 8)),
  cursor:reset(Cursor),
  ?assertEqual(11, cursor:get(Cursor, "persist_id")),
  ?assertEqual(547732, cursor:get(Cursor, "int_col")),

  ?PASSED
end.

cursor_foreach(_X, Result) -> fun() ->
  Cursor = cursor:new(Result),
  L = cursor:foreach(Cursor, "varchar_col"),
  ?assertEqual("Varchar value", lists:nth(2, L)),
  L1 = cursor:foreach(Cursor, "persist_id"),
  ?assertEqual(lists:seq(11, 37), L1),
  ?PASSED
end.

cursor_new(_X, Result) -> fun() ->
  Cursor = cursor:new(Result),
  ?assertEqual(27, cursor:size(Cursor)),
  cursor:new(cursorName, Result),
  ?assertEqual(27, cursor:size(cursorName)),
  ?PASSED
end.

cursor_error(_X, Result) -> fun() ->
  Cursor = cursor:new(Result),
  Cursor ! {self(), sizze},
  Response = receive
    Resp -> Resp
  end,
  ?assertEqual(unknown_operation, Response),

  Self = self(),
  spawn(fun() -> ?assertEqual(access_forbidden, cursor:size(Cursor)), Self ! continue end),
  receive
    continue -> ok
  end,
  ?PASSED
end.

