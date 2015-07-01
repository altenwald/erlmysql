%%
%% Copyright (C) 2011 by krasnop@bellsouth.net (Alexei Krasnopolski)
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
%% @since 2010-12-02
%% @copyright 2010-2011 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc This module is running erlang unit tests.

-module(my_debug).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("client_records.hrl").
-include("mysql_types.hrl").
-include("test.hrl").

-export([blob/0, debug/0, test_compress/0, statement_long_parameter_send/0,
				zero_length_response/0,
				test_fetch/0, get_field_list/0]).

%%
%% API Functions
%%
big_packet_test() ->
  my:start_client(),
  DS_def = #datasource{
    host = "localhost", 
    port = 3306, 
    database = "eunitdb", 
    user = "root", 
    password = "root", 
%    flags = #client_options{}
    flags = #client_options{trans_isolation_level = serializable}
  },
  my:new_datasource(data_source, DS_def),
  io:format("~n before get connection!~n"),
  Conn = datasource:get_connection(data_source),
  io:format("~n after get connection~n"),
    
  {_,R1} = connection:execute_query(Conn, "select * from fgamedb.t_coin_diamond_consume"),
  
%  Query1 = "INSERT INTO all_in_one (int_col) VALUES (7)",
%  Query1 = "SELECT * FROM all_in_one WHERE persist_id = 1",
%  Handle = connection:get_prepared_statement_handle(Conn, Query1),
%  {_,[R1|_]} = connection:execute_statement(Conn, Handle, 
%      [], 
%      []),
 ?debug_Fmt("~p~n", [length(R1)])
%  ?assert(is_record(R1, ok_packet))
  
.

zero_length_response() ->
	?debug_Msg(" >>> zero_length_response()"),
	Connection = my:open_connection(?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, "eunitdb", "root", "root"),
	Query = "UPDATE all_in_one SET longtext_col= ? WHERE persist_id = ?;",
	Handle = my:get_prepared_statement_handle(Connection, Query),

	{_,[R|_]} = my:execute_statement(Connection, Handle, [?LONG_BLOB, ?LONGLONG], ["", 1]),
	?assert(is_record(R, ok_packet)),

	my:close_statement(Connection, Handle),

	Query1 = "SELECT longtext_col FROM all_in_one WHERE persist_id=",
	{_,[R1|_]} = my:execute_query(Connection, Query1 ++ "1"),
	?assert(is_list(R1)),
	[Field_1 | _] = R1,
	?debug_Fmt(".~p.~n", [Field_1]),

	?assertEqual(0, length(Field_1)),
	
	Query2 = "SELECT longtext_col FROM all_in_one WHERE persist_id= ?;",
	Handle2 = my:get_prepared_statement_handle(Connection, Query2),

	{_,[R2|_]} = my:execute_statement(Connection, Handle2, [?LONGLONG], [1]),
	?assert(is_list(R2)),
	[Field_2 | _] = R2,
	?debug_Fmt(".~p.~n", [Field_2]),

	?assertEqual(0, length(Field_2)),

	my:close_statement(Connection, Handle2),

	my:close_connection(Connection)
.

for(0, _) -> ok;
for(N, F) -> F(N), for(N-1, F).

for_bin(1, F) -> F(1);
for_bin(N, F) -> list_to_binary([F(N) | for_bin(N-1, F)]).

blob() -> 
%	?debug_Msg(" >>> blob()"),
	L = 16#100000 - 64, % 16#100000 - max alowed packet size
	B = for_bin(L div 64, fun(M) -> <<M:512/integer>> end),
	blob(0, B), 
	blob(1, B)
.

blob(C, B) ->
	?debug_Fmt(" >>> 12. blob(~p)",[C]),
	Connection = my:open_connection(?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, 
																	"eunitdb", "root", "root", #client_options{compress=C}),
	Query = "INSERT INTO all_in_one (longblob_col) VALUES (?)",
	Handle = my:get_prepared_statement_handle(Connection, Query),
	L = size(B),
	K = 20,
	
	?debug_Msg(" >>> send long parameter"),
	for(K, fun(_M) -> my:send_statement_long_parameter(Connection, Handle, 0, B) end),

	{_,[R|_]} = my:execute_statement(Connection, Handle, [?LONG_BLOB], [null]),
	?assert(is_record(R, ok_packet)),
	Id = R#ok_packet.insert_id,
%	?debug_Fmt("L: ~p Id: ~p~n", [L*K, Id]),
	my:close_statement(Connection, Handle),

	Query1 = ["SELECT longblob_col FROM all_in_one WHERE persist_id=", integer_to_list(Id)],
	{_,[R1|_]} = my:execute_query(Connection, Query1),
	[Field_1 | _] = R1, 
	?assertEqual(L*K, size(Field_1)),
%	?debug_Fmt("L: ~p ~n", [size(Field_1)]),

	Query2 = "SELECT longblob_col FROM all_in_one WHERE persist_id=?",
	Handle2 = my:get_prepared_statement_handle(Connection, Query2),
	{_,[R2|_]} = my:execute_statement(Connection, Handle2, [?LONGLONG], [Id]),
	[Field_2 | _] = R2,
	?assertEqual(L*K, size(Field_2)),
%	?debug_Fmt("L: ~p ~n", [size(Field_2)]),

	my:close_statement(Connection, Handle2),
	my:close_connection(Connection)
.

statement_long_parameter_send() ->
	?debug_Msg(" >>> statement_long_parameter_send()"),
	Connection = my:open_connection(?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, "eunitdb", "root", "root"),
	Query = "UPDATE all_in_one SET int_col= ?, longblob_col= ? WHERE persist_id = ?;",
	Handle = my:get_prepared_statement_handle(Connection, Query),

	my:send_statement_long_parameter(Connection, Handle, 1, <<"Test of sending long par">>),
	my:send_statement_long_parameter(Connection, Handle, 1, <<"ameter!!!!">>),
	
	{_,[R|_]} = my:execute_statement(Connection, Handle, [?SHORT, ?LONG_BLOB, ?LONGLONG], [5, null, 1]),
	?assert(is_record(R, ok_packet)),

	my:close_statement(Connection, Handle),

	Query1 = "SELECT longblob_col FROM all_in_one WHERE persist_id=",
	{_,[R1|_]} = my:execute_query(Connection, Query1 ++ "1"),
	?assert(is_list(R1)),
	[Field_1 | _] = R1,
	Expected = <<"Test of sending long parameter!!!!">>,
	?assertEqual(Expected, Field_1),

	my:close_connection(Connection)
.

%% debug() ->
%% 	Connection = my:open_connection(?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, "eunitdb", "root", "root"),
%% 	{_,R} = my:get_field_list(Connection, "all_in_one", "datet%"),
%% 	io:format("Field List = ~n~p~n", [R]),
%% 	my:close_connection(Connection)
%% .
debug() ->
	?debug_Msg(" >>> 10. statementNullBitMap()"),
	Connection = my:open_connection(?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, "eunitdb", "root", "root"),
	Query1 = ["INSERT INTO all_in_one (int_col,varchar_col,decimal_34_14_col,date_col,"
	, "float_col,bit_col,tinyblob_col,longtext_col,enum_col,smallint_col,char_col,year_col,tinyint_col) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"],
	{_,[R|_]} = my:prepare_statement(Connection, Query1),
	?debug_Fmt("~p~n", [R]),
	?assert(is_record(R, ok_stmt_packet)),
	Handle = R#ok_stmt_packet.stmt_handle,
	{_,[R1|_]} = my:execute_statement(Connection, Handle, 
			[?LONG, ?VARCHAR, ?DECIMAL, ?DATE, ?FLOAT, ?BIT, ?TINY_BLOB, ?NULL, ?ENUM, ?TINY, ?NULL,?YEAR,?TINY], 
			[1547732,null,#mysql_decimal{int="11024567", fraction="3457821"},#mysql_time{year=2011,month=12,day=25,hour=23,minute=55},
				11.45E-7,<<0:1,1:1,1:1,0:1,1:1,1:1,1:1,0:1,1:1,0:1,1:1>>,<<"Tiny Blob object.">>,
				<<"Long Text object.">>,"one",7,"character",2012,1]),
	?debug_Fmt("~p~n", [R1]),
	?assert(is_record(R1, ok_packet)),

	{_,[R2|_]} = my:prepare_statement(Connection, "SELECT * FROM all_in_one WHERE persist_id = ?"),
	?debug_Fmt("~p~n", [R2]),
	?assert(is_record(R2, ok_stmt_packet)),
	Handle1 = R2#ok_stmt_packet.stmt_handle,
	{_,[R3|_]} = my:execute_statement(Connection, Handle1, [?LONGLONG], [R1#ok_packet.insert_id]),
	?debug_Fmt("~p~n", [R3]),
	?assert(is_list(R3)),

	Expected = [R1#ok_packet.insert_id,1547732,null,{mysql_decimal,"11024567","34578210000000"},null,
		{mysql_time,false,2011,12,25,0,0,0,0},
		null,null,2012,null,null,1.1450000556578743e-6,null,7,
		1,null,<<3,117>>,"one",null,null,null,null,null,<<"Tiny Blob object.">>,null,
		null,null,null,null],
	?assertEqual(Expected, R3),
	my:close_statement(Connection, Handle),
	my:close_connection(Connection)
.

test_compress() ->
	?debug_Msg(" >>> test_compress()"),
	Connection = my:open_connection(?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, 
			"eunitdb", "root", "root", #client_options{compress=1}),
	?debug_Fmt("Connection= ~p~n Compress= ~p~n", [Connection,Connection#connection.ds_def#datasource.flags#client_options.compress]),

	{_,R} = my:execute_query(Connection, "SELECT * FROM all_in_one WHERE persist_id = 1 or persist_id = 2;"),
	?debug_Fmt("~p~n", [R]),
%	{_,[R|_]} = my:ping(Connection),
%	?assert(is_list(R)),

	my:close_connection(Connection)
.

test_fetch() ->
	?debug_Msg(" >>> test_compress()"),
	Connection = my:open_connection(?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, 
			"eunitdb", "root", "root", #client_options{compress=0}),

	?debug_Fmt("prepare: ~n", []),
	{MD,[RS|RSS]} = my:prepare_statement(Connection, 
		"SELECT int_col,varchar_col,decimal_34_14_col,date_col FROM all_in_one WHERE persist_id < ?"),
	?debug_Fmt("after prepare: ~p~n~p~n~p~n", [MD,RS,RSS]),
	?assert(is_record(MD, metadata)),
	?assertEqual(4, MD#metadata.field_count),
	?assertEqual(1, MD#metadata.param_count),
	?assert(is_record(RS, ok_stmt_packet)),
	Handle = RS#ok_stmt_packet.stmt_handle,
	?debug_Fmt("execute: ~n", []),
	case my:execute_statement(Connection, Handle, [?LONGLONG], [5], ?CURSOR_TYPE_READ_ONLY, true) of
		{M, []} -> R1 = [];
		{M, Rows} ->
			[R1|_] = Rows,
			?assert(is_record(M, metadata)),
			?assertEqual(4, M#metadata.field_count),
			?assertEqual(0, M#metadata.param_count),
			?assert(is_list(R1)),
			?assertEqual(4, length(Rows))
	end,
	?debug_Fmt("execute metadata: ~p~n", [M]),
	?debug_Fmt("execute result: ~p~n", [R1]),
	
	{M2,R2} = my:fetch_statement(Connection, Handle, M, 2),
	?debug_Fmt("fetch: ~p~n", [R2]),
	?assert(is_record(M2, metadata)),
	?assertEqual(4, M2#metadata.field_count),
	?assertEqual(0, M2#metadata.param_count),
	[R3 | _] = R2,
	?assert(is_list(R3)),
	?assertEqual(2, length(R2)),
	
	my:close_statement(Connection, Handle),
	my:close_connection(Connection)
.

get_field_list() ->
	?debug_Fmt(" >>> 9. get_field_list(~p)",[0]),
	Connection = my:open_connection(?TEST_SERVER_HOST_NAME, ?TEST_SERVER_PORT, "eunitdb", "root", "root", #client_options{compress=0}),
	?debug_Msg("----------------"),
	{#metadata{field_metadata = M},[]} = my:get_field_list(Connection, "all_in_one", "%"), 
%	?debug_Fmt("Field List = ~p; ~n", [M]),
%	?debug_Fmt("F = ~p; FC=~p~n", [F, FC]),
	[R | _] = M,
	?assert(is_record(R, field_metadata)),
	?assertEqual(30, length(M)),
	my:close_connection(Connection)
.

