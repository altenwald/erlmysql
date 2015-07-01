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

-module(statement_tests).

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

statement_test_() ->
	[ 
    { foreachx, 
      fun testing:do_setup/1, 
      fun testing:do_cleanup/2, 
      [
        {{plain, statementSelect},    fun statementSelect/2},
        {{compress, statementSelect}, fun statementSelect/2},
        {{plain, statementSelect_1}, fun statementSelect_1/2},
        {{compress, statementSelect_1}, fun statementSelect_1/2},
        {{plain, statementInsert}, fun statementInsert/2},
        {{compress, statementInsert}, fun statementInsert/2},
        
        {{plain, statementInsertOne}, fun statementInsertOne/2},
        {{compress, statementInsertOne}, fun statementInsertOne/2},

        {{plain, statementInsertAll}, fun statementInsertAll/2},
        {{compress, statementInsertAll}, fun statementInsertAll/2},
        {{plain, statementNullBitMap}, fun statementNullBitMap/2},
        {{compress, statementNullBitMap}, fun statementNullBitMap/2},
        {{plain, statementParameterCheck}, fun statementParameterCheck/2},
        {{compress, statementParameterCheck}, fun statementParameterCheck/2},
        {{plain, statement_long_parameter_send}, fun statement_long_parameter_send/2},
        {{compress, statement_long_parameter_send}, fun statement_long_parameter_send/2},
        {{plain, statement_test_fetch}, fun statement_test_fetch/2},
        {{compress, statement_test_fetch}, fun statement_test_fetch/2}
      ]
    }
	].

%%
%% Local Functions
%%

statementSelect(_X, C) -> fun() ->
	Connection = datasource:get_connection(C),
	{M,[R|_]} = connection:prepare_statement(Connection, "SELECT * FROM all_in_one WHERE persist_id = ?"),
%	?debug_Fmt("~p~n", [R]),
	?assert(is_record(M, metadata)),
	?assertEqual(30, M#metadata.field_count),
	?assertEqual(1, M#metadata.param_count),
	?assert(is_record(R, ok_stmt_packet)),
	Handle = R#ok_stmt_packet.stmt_handle,
	{M1,[R1|_]} = connection:execute_statement(Connection, Handle, [?LONGLONG], [1]),
%%	?debug_Fmt("~p~n", [R1]),
	?assert(is_record(M1, metadata)),
	?assertEqual(30, M1#metadata.field_count),
	?assertEqual(0, M1#metadata.param_count),
	?assert(is_list(R1)),
	Expected = [1,1025,null,{mysql_decimal,"12034879","00000000000000"},{mysql_time,false,1954,5,25,22,23,16,0},
		{mysql_time,false,1977,7,9,0,0,0,0},{mysql_time,false,0,0,0,41,35,47,0},
		{mysql_time,false,2000,12,31,23,59,59,0},1977,{mysql_decimal,"-120334765","0050500"},12005.3451,456.3299865722656,
		212121,2141,717,21,"characters: char[25]",null,null,[one,two],
		<<66,105,110,97,114,121,40,50,53,48,41,58,32,66,105,110,97,
			114,121,32,97,114,114,97,121,32,61,32,65,65,65,65,65,65,65,
			65,65,65,65,65,65,65,65,44,32,66,66,66,66,66,66,66,66,66,
			66,66,66,66,66,66,66,66,66,66,66,66,44,32,74,74,74,74,74,
			74,74,74,74,74,74,74,74,74,74,74,74,74,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
		<<0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,
			55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,
			0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,
			55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,
			0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55,0,55>>,
		<<"This is a LONG BLOB object.">>,
		<<"This is a MEDIUM BLOB object.">>,
		<<"This is a TINY BLOB object.">>,
		<<"VARBINARY(1000): EMPTY EMPTY EMPTY.">>,
		"This is a TINYTEXT object.",null,null,
		[]],

	?assertEqual(Expected, R1),
	connection:close_statement(Connection, Handle),
	datasource:return_connection(C, Connection),
  ?PASSED
end.

statementSelect_1(_X, C) -> fun() ->
	Connection = datasource:get_connection(C),
	{M, RA} = connection:prepare_statement(Connection, 
		"SELECT int_col, char_col, year_col, enum_col FROM all_in_one WHERE persist_id = ? or char_col = ? or int_col = ?"),
%	?debug_Fmt("~p~n", [M]),
%	?debug_Fmt("~p~n", [RA]),
	[R | _] = RA,
	?assert(is_record(M, metadata)),
	?assertEqual(4, M#metadata.field_count),
	?assertEqual(3, M#metadata.param_count),
	?assert(is_record(R, ok_stmt_packet)),
	Handle = R#ok_stmt_packet.stmt_handle,
	{M1,RA1} = connection:execute_statement(Connection, Handle, [?LONGLONG, ?STRING, ?LONG], [1,"char",1025]),
%	?debug_Fmt("~p~n", [M1]),
%	?debug_Fmt("~p~n", [RA1]),
	[R1 | _] = RA1,
	?assert(is_record(M1, metadata)),
	?assertEqual(4, M1#metadata.field_count),
	?assertEqual(0, M1#metadata.param_count),
	?assert(is_list(R1)),
	Expected = [1025,"characters: char[25]",1977,null],

	?assertEqual(Expected, R1),
	connection:close_statement(Connection, Handle),
	datasource:return_connection(C, Connection),
  ?PASSED
end.

statementInsert(_X, C) -> fun() ->
	Connection = datasource:get_connection(C),
	Query1 = ["INSERT INTO all_in_one (int_col,varchar_col,decimal_34_14_col,date_col,"
	, "float_col,bit_col,tinyblob_col,longtext_col) VALUES (?,?,?,?,?,?,?,?)"],
	{M,[R|_]} = connection:prepare_statement(Connection, Query1),
%	?debug_Fmt("~p~n", [R]),
	?assert(is_record(M, metadata)),
	?assertEqual(0, M#metadata.field_count),
	?assertEqual(8, M#metadata.param_count),
	?assert(is_record(R, ok_stmt_packet)),
	connection:close_statement(Connection, R#ok_stmt_packet.stmt_handle),

	Handle = connection:get_prepared_statement_handle(Connection, Query1),
	{_,[R1|_]} = connection:execute_statement(Connection, Handle, 
			[?LONG, ?VARCHAR, ?DECIMAL, ?DATE, ?FLOAT, ?BIT, ?TINY_BLOB, ?LONG_BLOB], 
			[547732,"Varchar value",#mysql_decimal{int="1024567", fraction="345782"},#mysql_time{year=2010,month=12,day=25,hour=23,minute=55},
				10.45E-7,<<0:1,1:1,1:1,0:1,1:1,1:1,1:1,0:1,1:1,0:1,1:1>>,<<"Tiny Blob object.">>,
				<<"Long Text object.">>]),
%	?debug_Fmt("~p~n", [R1]),
	?assert(is_record(R1, ok_packet)),

	{_,[R2|_]} = connection:prepare_statement(Connection, "SELECT * FROM all_in_one WHERE persist_id = ?"),
%	?debug_Fmt("~p~n", [R2]),
	?assert(is_record(R2, ok_stmt_packet)),
	Handle1 = R2#ok_stmt_packet.stmt_handle,
	{_,[R3|_]} = connection:execute_statement(Connection, Handle1, [?LONGLONG], [R1#ok_packet.insert_id]),
%	?debug_Fmt("~p~n", [R3]),
	?assert(is_list(R3)),

	Expected = [R1#ok_packet.insert_id,547732,"Varchar value",{mysql_decimal,"1024567","34578200000000"},null,
		{mysql_time,false,2010,12,25,0,0,0,0},
		null,null,null,null,null,1.0449999763295637e-6,null,null,
		null,null,null,<<110,5:3>>,null,null,null,null,null,null,<<"Tiny Blob object.">>,null,
		null,null,null,"Long Text object."],
	?assertEqual(Expected, R3),
	connection:close_statement(Connection, Handle),
	datasource:return_connection(C, Connection),
  ?PASSED
end.

statementInsertOne(_X, C) -> {timeout, 30, fun() ->
	Connection = datasource:get_connection(C),
	FieldNames = [
		"int_col","varchar_col","decimal_34_14_col","datetime_col","date_col",
		"time_col","timestamp_col","year_col","decimal_24_7_col",
    "double_col","float_col","double_col","float_col","double_col","float_col",
    "mediumint_col","smallint_col","tinyint_col","char_col",
		"bit_col","enum_col","set_col","binary_col","blob_col",
		"longblob_col","mediumblob_col","tinyblob_col","varbinary_col","tinytext_col",
		"text_col","mediumtext_col","longtext_col",
		"varchar_col","varbinary_col", %% additionaly repeate fields with others declared types 
		"decimal_34_14_col","mediumint_col","smallint_col","tinyint_col", "usmallint_col"
		,"int_col","mediumint_col","smallint_col","tinyint_col", "usmallint_col" %% negative values
	],
	FieldTypes = [
		?LONG, ?VAR_STRING, ?DECIMAL, ?DATETIME, ?DATE,
		?TIME, ?TIMESTAMP, ?YEAR, ?NEWDECIMAL,
    ?DOUBLE, ?FLOAT, ?DOUBLE, ?FLOAT, ?DOUBLE, ?FLOAT, 
    ?INT24, ?SHORT, ?TINY, ?STRING,
		?BIT, ?ENUM, ?SET, ?STRING, ?BLOB,
		?LONG_BLOB, ?MEDIUM_BLOB, ?TINY_BLOB, ?VAR_STRING, ?TINY_BLOB,
		?BLOB, ?MEDIUM_BLOB, ?LONG_BLOB,
		?VARCHAR, ?STRING,
		?NEWDECIMAL, ?TINY, ?INT24, ?LONG, (?MYSQL_TYPE_SHORT + ?MYSQL_TYPE_UNSIGNED),
		?LONGLONG, ?LONG, ?SHORT, ?TINY, (?MYSQL_TYPE_SHORT + ?MYSQL_TYPE_UNSIGNED)
	], 
	FieldValues = [
		547732,"Varchar value",#mysql_decimal{int="1024567", fraction="34578200000000"},
				#mysql_time{year=2010,month=12,day=25,hour=23,minute=55},#mysql_time{year=1983, month=3, day=25},
		#mysql_time{neg=true,hour=200,minute=45,second=33},#mysql_time{year=1980,month=12,day=25,hour=23,minute=55},
				2023,#mysql_decimal{int="979797", fraction="0003203"},
    -23.875547E-32, 10.45E-7, 345.17, -76.78, 0.0, 7.0,
    25625,255,7,"char[20]",
		<<0:1,1:1,1:1,0:1,1:1,1:1,1:1,0:1,1:1,0:1,1:1>>,one,[one,two],<<33,0," Binary ">>,<<"Blob object.">>,
		<<"Long Blob object.">>,<<"Medium Blob object.">>,<<"Tiny Blob object.">>,<<3,0," VarBinary ">>,
				"Tiny Text object.",
		"Text object.","Medium Text object.","Long Text object.",
		"Varchar as a varchar", <<"Varbinary as a string">>,
		#mysql_decimal{int="1724767", fraction="34578200000000"}, 25, 10256, 8, 17,
		-1000256000, -167773, -3000, -128, 65535 
	],
	statementInsertOne(Connection, FieldNames, FieldTypes, FieldValues),
	datasource:return_connection(C, Connection),
  ?PASSED
end}.

statementInsertOne(_, [], _, _) -> ok;
statementInsertOne(Connection, [FieldName | RestFieldNames], [FieldType | RestFieldTypes], [FieldValue | RestFieldValues]) ->
	Conn_record = connection:connection_record(Connection),
	?debug_Fmt(" >>> Statement 4. statementInsertOne(compress=~p) for ~p", 
							[Conn_record#connection.ds_def#datasource.flags#client_options.compress, FieldName]),
	Query1 = "INSERT INTO all_in_one (" ++ FieldName ++ ") VALUES (?)",
	{M,[R|_]} = connection:prepare_statement(Connection, Query1),
%	?debug_Fmt("~p~n", [R]),
	?assert(is_record(M, metadata)),
	?assertEqual(0, M#metadata.field_count),
	?assertEqual(1, M#metadata.param_count),
	?assert(is_record(R, ok_stmt_packet)),
	Handle = R#ok_stmt_packet.stmt_handle,
	{M1,[R1|_]} = connection:execute_statement(Connection, Handle, [FieldType], [FieldValue]),
%	?debug_Fmt("~p~n", [R1]),
	?assert(is_record(M1, metadata)),
	?assertEqual(0, M1#metadata.field_count),
	?assertEqual(0, M1#metadata.param_count),
	?assert(is_record(R1, ok_packet)),

	{M2,[R2|_]} = connection:prepare_statement(Connection, "SELECT " ++ FieldName ++ " FROM all_in_one WHERE persist_id = ?"),
%	?debug_Fmt("~p~n", [R2]),
	?assert(is_record(M2, metadata)),
	?assertEqual(1, M2#metadata.field_count),
	?assertEqual(1, M2#metadata.param_count),
	?assert(is_record(R2, ok_stmt_packet)),
	Handle1 = R2#ok_stmt_packet.stmt_handle,
	{M3,[R3|_]} = connection:execute_statement(Connection, Handle1, [?LONGLONG], [R1#ok_packet.insert_id]),
%	?debug_Fmt("~p~n", [R3]),
	?assert(is_record(M3, metadata)),
	?assertEqual(1, M3#metadata.field_count),
	?assertEqual(0, M3#metadata.param_count),
	?assert(is_list(R3)),

	[V1] = R3,
	case FieldName of
		_ when is_float(FieldValue) =:= true -> 
%     ?debug_Fmt("float value: expected=~p; value=~p", [FieldValue, V1]),
        if (abs((FieldValue - V1)/FieldValue) < 1.0E-6) -> ?assert(true);
           true -> ?assertEqual(FieldValue, V1)
        end;
		"binary_col" -> 
%			?debug_Fmt("binary_col value: expected=~p~nvalue=~p~n", [FieldValue, V1]),
			Sz = byte_size(FieldValue),
			<<V2:Sz/binary, _/binary>> = V1,
			?assertEqual(FieldValue, V2);
%		"enum_col" -> % ?debug_Fmt("enum_col value: ~p~n", [V1]), 
%				?assertEqual(atom_to_list(FieldValue), V1);
%		"set_col" -> % ?debug_Fmt("set_col value: ~p~n", [V1]),
%				?assertEqual(statement:atom_list_to_string(FieldValue), V1);
%% 		"varbinary_col" -> {V2,_} = lists:split(length(FieldValue), V1),
%% 										?assertEqual(FieldValue, V2);
		_ -> ?assertEqual(FieldValue, V1)
	end,
	connection:close_statement(Connection, Handle),
	connection:close_statement(Connection, Handle1),
	statementInsertOne(Connection, RestFieldNames, RestFieldTypes, RestFieldValues).

statementInsertAll(_X, C) -> fun() ->
	Connection = datasource:get_connection(C),
	Query1 = ["INSERT INTO all_in_one ("
		"int_col,varchar_col,decimal_34_14_col,datetime_col,date_col,",
		"time_col,timestamp_col,year_col,decimal_24_7_col,double_col,",
		"float_col,mediumint_col,smallint_col,tinyint_col,char_col,",
		"bit_col,enum_col,set_col,binary_col,blob_col,",
		"longblob_col,mediumblob_col,tinyblob_col,varbinary_col,tinytext_col,",
		"text_col,mediumtext_col,longtext_col,usmallint_col",
		") VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"],
	{M,[R|_]} = connection:prepare_statement(Connection, Query1),
%	?debug_Fmt("~p~n", [R]),
	?assert(is_record(M, metadata)),
	?assertEqual(0, M#metadata.field_count),
	?assertEqual(29, M#metadata.param_count),
	?assert(is_record(R, ok_stmt_packet)),
	Handle = R#ok_stmt_packet.stmt_handle,
	{_,[R1|_]} = connection:execute_statement(Connection, Handle, 
			[?LONG, ?VAR_STRING, ?DECIMAL, ?DATETIME, ?DATE, 
				?TIME, ?TIMESTAMP, ?YEAR, ?NEWDECIMAL, ?DOUBLE,
				?FLOAT, ?INT24, ?SHORT, ?TINY, ?VAR_STRING,
				?BIT, ?ENUM, ?SET, ?VAR_STRING, ?BLOB,
				?LONG_BLOB, ?MEDIUM_BLOB, ?TINY_BLOB, ?VAR_STRING, ?TINY_BLOB,
				?BLOB, ?MEDIUM_BLOB, ?LONG_BLOB, ?INT24], 
 			[547732,"Varchar value",#mysql_decimal{int="1024567", fraction="345782"},
				#mysql_time{year=2010,month=12,day=25,hour=23,minute=55},#mysql_time{year=1983, month=3, day=25},
				#mysql_time{neg=true,hour=200,minute=45,second=33},#mysql_time{year=1980,month=12,day=25,hour=23,minute=55},
				23,#mysql_decimal{int="979797", fraction="00032032"},23.875547E-32,
				10.45E-7,256256,255,7,"char[20]",
				<<0:1,1:1,1:1,0:1,1:1,1:1,1:1,0:1,1:1,0:1,1:1>>,one,[one,two],[33,0|" Binary "],<<"Blob object.">>,
				<<"Long Blob object.">>,<<"Medium Blob object.">>,<<"Tiny Blob object.">>,[3,0|" VarBinary "],
				<<"Tiny Text object.">>,<<"Text object.">>,<<"Medium Text object.">>,<<"Long Text object.">>,
				717]),
%	?debug_Fmt("~p~n", [R1]),
	?assert(is_record(R1, ok_packet)),

	{_,[R2|_]} = connection:prepare_statement(Connection, "SELECT * FROM all_in_one WHERE persist_id = ?"),
%	?debug_Fmt("~p~n", [R2]),
	?assert(is_record(R2, ok_stmt_packet)),
	Handle1 = R2#ok_stmt_packet.stmt_handle,
	{_,[R3|_]} = connection:execute_statement(Connection, Handle1, [?LONGLONG], [R1#ok_packet.insert_id]),
%	?debug_Fmt("~p~n", [R3]),
	?assert(is_list(R3)),

	Expected = [R1#ok_packet.insert_id,547732,"Varchar value",
		{mysql_decimal,"1024567","34578200000000"},
		{mysql_time,false,2010,12,25,23,55,0,0},
		{mysql_time,false,1983,3,25,0,0,0,0},
		{mysql_time,true,0,0,0,200,45,33,0},
		{mysql_time,false,1980,12,25,23,55,0,0},
		2023,
		{mysql_decimal,"979797","0003203"},
		2.3875547e-31,1.0449999763295637e-6,256256,255,717,7,"char[20]",
		<<110,5:3>>,
		one,[one,two],
		<<33,0,32,66,105,110,97,114,121,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
		<<"Blob object.">>,<<"Long Blob object.">>,<<"Medium Blob object.">>,
		<<"Tiny Blob object.">>,
		<<3,0,32,86,97,114,66,105,110,97,114,121,32>>,
		"Tiny Text object.","Text object.","Medium Text object.",
		"Long Text object."],
	?assertEqual(Expected, R3),
	connection:close_statement(Connection, Handle),
	connection:close_statement(Connection, Handle1),
	datasource:return_connection(C, Connection),
  ?PASSED
end.

statementNullBitMap(_X, C) -> fun() ->
	Connection = datasource:get_connection(C),
	Query1 = ["INSERT INTO all_in_one (int_col,varchar_col,decimal_34_14_col,date_col,"
	, "float_col,bit_col,tinyblob_col,longtext_col,enum_col,smallint_col,char_col,year_col,tinyint_col) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"],
	{_,[R|_]} = connection:prepare_statement(Connection, Query1),
%	?debug_Fmt("~p~n", [R]),
	?assert(is_record(R, ok_stmt_packet)),
	Handle = R#ok_stmt_packet.stmt_handle,
	{_,[R1|_]} = connection:execute_statement(Connection, Handle, 
			[?LONG, ?VARCHAR, ?DECIMAL, ?DATE, ?FLOAT, ?BIT, ?TINY_BLOB, ?NULL, ?ENUM, ?TINY, ?NULL,?YEAR,?TINY], 
			[1547732,null,#mysql_decimal{int="11024567", fraction="3457821"},#mysql_time{year=2011,month=12,day=25,hour=23,minute=55},
				11.45E-7,<<0:1,1:1,1:1,0:1,1:1,1:1,1:1,0:1,1:1,0:1,1:1>>,<<"Tiny Blob object.">>,
				<<"Long Text object.">>,one,7,"character",2012,1]),
%	?debug_Fmt("~p~n", [R1]),
	?assert(is_record(R1, ok_packet)),

	{_,[R2|_]} = connection:prepare_statement(Connection, "SELECT * FROM all_in_one WHERE persist_id = ?"),
%	?debug_Fmt("~p~n", [R2]),
	?assert(is_record(R2, ok_stmt_packet)),
	Handle1 = R2#ok_stmt_packet.stmt_handle,
	{_,[R3|_]} = connection:execute_statement(Connection, Handle1, [?LONGLONG], [R1#ok_packet.insert_id]),
%	?debug_Fmt("~p~n", [R3]),
	?assert(is_list(R3)),

	Expected = [R1#ok_packet.insert_id,1547732,null,{mysql_decimal,"11024567","34578210000000"},null,
		{mysql_time,false,2011,12,25,0,0,0,0},
		null,null,2012,null,null,1.1450000556578743e-6,null,7,null,
		1,null,<<110,5:3>>,one,null,null,null,null,null,<<"Tiny Blob object.">>,null,
		null,null,null,null],
	?assertEqual(Expected, R3),
	connection:close_statement(Connection, Handle),
	datasource:return_connection(C, Connection),
  ?PASSED
end.

mysql_err(Msg) ->
  #mysql_error{type = statement, source = "helper_statement:check_param/3", message = Msg}.

statementParameterCheck(_X, C) -> fun() ->
	Connection = datasource:get_connection(C),
	Fixtures = [
		{"int_col", ?UNSIGNED, "1", mysql_err("Parameter #1 is not integer.")},
		{"int_col", ?TINY + ?UNSIGNED, -2, mysql_err("Parameter #1 has unsigned type but negative.")},
		{"int_col", ?TINY, "3", mysql_err("Parameter #1 is not integer.")},		
		{"int_col", ?TINY, 128, mysql_err("Parameter #1 of TINY type is not in [127,-128].")},
		{"int_col", ?TINY + ?UNSIGNED, 256, mysql_err("Parameter #1 of UNSIGNED TINY type is not in [0,255].")},
		{"int_col", ?SHORT, -32769, mysql_err("Parameter #1 of SHORT type is not in [32767,-32768].")},
		{"int_col", ?SHORT + ?UNSIGNED, 65536, mysql_err("Parameter #1 of UNSIGNED SHORT type is not in [0,65535].")},
		{"int_col", ?INT24, 8388608, mysql_err("Parameter #1 of INT24 type is not in [8388607,-8388608].")},
		{"int_col", ?INT24 + ?UNSIGNED, 16777217, mysql_err("Parameter #1 of UNSIGNED INT24 type is not in [0,16777215].")},
		{"int_col", ?LONG, 2147483648, mysql_err("Parameter #1 of LONG type is not in [2147483647,-2147483648].")},
		{"int_col", ?LONG + ?UNSIGNED, 4294967296, mysql_err("Parameter #1 of UNSIGNED LONG type is not in [0,4294967295].")},
		{"int_col", ?LONGLONG, -9223372036854775809, mysql_err("Parameter #1 of LONGLONG type is not in [9223372036854775807,-9223372036854775808].")},
		{"int_col", ?LONGLONG + ?UNSIGNED, 18446744073709551616, mysql_err("Parameter #1 of UNSIGNED LONGLONG type is not in [0,18446744073709551615].")},
		{"int_col", ?FLOAT, 14, mysql_err("Parameter #1 is not float.")},
		{"int_col", ?DOUBLE, 15, mysql_err("Parameter #1 is not float.")},
		{"int_col", ?TIME, 16, mysql_err("Parameter #1 is not #mysql_time record.")},
		{"int_col", ?TIMESTAMP, 17, mysql_err("Parameter #1 is not #mysql_time record.")},
		{"int_col", ?DATETIME, 18, mysql_err("Parameter #1 is not #mysql_time record.")},
		{"int_col", ?DATE, 19, mysql_err("Parameter #1 is not #mysql_time record.")},
		{"int_col", ?YEAR, "2011", mysql_err("Parameter #1 is not integer.")},
		{"int_col", ?VARCHAR, 2.0e-4, mysql_err("Parameter #1 is not string.")},
		{"int_col", ?ENUM, 22, mysql_err("Parameter #1 is not atom.")},
		{"int_col", ?SET, set, mysql_err("Parameter #1 is not list.")},
		{"int_col", ?DECIMAL, decimal, mysql_err("Parameter #1 is not #mysql_decimal record.")},
		{"int_col", ?NEWDECIMAL, new_decimal, mysql_err("Parameter #1 is not #mysql_decimal record.")},
		{"int_col", ?BIT, bit, mysql_err("Parameter #1 is not bitstring.")},
		{"int_col", ?BLOB, blob, mysql_err("Parameter #1 is not binary or string.")}
	],
	parameterCheck(Connection, Fixtures),
	datasource:return_connection(C, Connection),
  ?PASSED
end.

parameterCheck(_Connection, []) -> ok;
parameterCheck(Connection, [{Field_name, Type, Value, Exception} | Rest]) -> 
	Query = ["INSERT INTO all_in_one (", Field_name, ") VALUES (?)"],
	{_,[R|_]} = connection:prepare_statement(Connection, Query),
%	?debug_Fmt("~p~n", [R]),
	?assert(is_record(R, ok_stmt_packet)),
	Handle = R#ok_stmt_packet.stmt_handle,
	?assertEqual(connection:execute_statement(Connection, Handle, [Type], [Value]), Exception),
%%	?assert(is_record(connection:execute_statement(Connection, Handle, [Type], [Value]), mysql_error)),
	connection:close_statement(Connection, Handle),
	parameterCheck(Connection, Rest).

statement_long_parameter_send(_X, C) -> fun() ->
	Connection = datasource:get_connection(C),
	Query = "UPDATE all_in_one SET int_col= ?, longblob_col= ? WHERE persist_id = ?;",
	Handle = connection:get_prepared_statement_handle(Connection, Query),

	connection:send_statement_long_parameter(Connection, Handle, 1, <<"Test of sending long par">>),
	connection:send_statement_long_parameter(Connection, Handle, 1, <<"ameter!!!!">>),
	
	{_,[R|_]} = connection:execute_statement(Connection, Handle, [?SHORT, ?LONG_BLOB, ?LONGLONG], [5, null, 1]),
	?assert(is_record(R, ok_packet)),

	connection:close_statement(Connection, Handle),

	Query1 = "SELECT longblob_col FROM all_in_one WHERE persist_id=",
	{_,[R1|_]} = connection:execute_query(Connection, Query1 ++ "1"),
	?assert(is_list(R1)),
	[Field_1 | _] = R1,
	Expected = <<"Test of sending long parameter!!!!">>,
	?assertEqual(Expected, Field_1),

	datasource:return_connection(C, Connection),
  ?PASSED
end.

statement_test_fetch(_X, C) -> fun() ->
	Connection = datasource:get_connection(C),

	{MD,[RS|_RSS]} = connection:prepare_statement(Connection, 
		"SELECT int_col,varchar_col,decimal_34_14_col,date_col FROM all_in_one WHERE persist_id < ?"),
	?assert(is_record(MD, metadata)),
	?assertEqual(4, MD#metadata.field_count),
	?assertEqual(1, MD#metadata.param_count),
	?assert(is_record(RS, ok_stmt_packet)),
	Handle = RS#ok_stmt_packet.stmt_handle,
	case connection:execute_statement(Connection, Handle, [?LONGLONG], [5], ?CURSOR_TYPE_READ_ONLY, true) of
		{M, []} -> _R1 = [];
		{M, Rows} ->
			[R1|_] = Rows,
			?assert(is_record(M, metadata)),
			?assertEqual(4, M#metadata.field_count),
			?assertEqual(0, M#metadata.param_count),
			?assert(is_list(R1)),
			?assertEqual(4, length(Rows))
	end,
	
	{M2,R2} = connection:fetch_statement(Connection, Handle, M, 2),
	?assert(is_record(M2, metadata)),
	?assertEqual(4, M2#metadata.field_count),
	?assertEqual(0, M2#metadata.param_count),
	[R3 | _] = R2,
	?assert(is_list(R3)),
	?assertEqual(2, length(R2)),
	
	connection:close_statement(Connection, Handle),
	datasource:return_connection(C, Connection),
  ?PASSED
end.
