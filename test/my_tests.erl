%%
%% Copyright (C) 2011-2013 by krasnop@bellsouth.net (Alexei Krasnopolski)
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
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc This module is running erlang unit tests.

-module(my_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("client_records.hrl").
-include("mysql_types.hrl").
-include("test.hrl").

%%
%% API Functions
%%
mysql_test_() ->
  [ 
    { setup, 
      fun testing:do_start/0, 
      fun testing:do_stop/1, 
      [
        {module, helper_common_tests},
        { foreachx, 
          fun testing:do_setup/1, 
          fun testing:do_cleanup/2, 
          [
            {{plain, 'DB_create_Table_create'},    fun(_X, Y) -> fun() -> dbCreate(Y), tableCreate(Y), ?PASSED end end},
            {{compress, 'DB_create_Table_create'}, fun(_X, Y) -> fun() -> dbCreate(Y), tableCreate(Y), ?PASSED end end},
            {{plain, tableInsert},     fun tableInsert/2},
            {{compress, tableInsert},  fun tableInsert/2},
            {{plain, tableSelect1},    fun tableSelect1/2},
            {{compress, tableSelect1}, fun tableSelect1/2},
            {{plain, tableSelect2},    fun tableSelect2/2},
            {{compress, tableSelect2}, fun tableSelect2/2},
            {{plain, tableSelect3},    fun tableSelect3/2},
            {{compress, tableSelect3}, fun tableSelect3/2},
            {{plain, tableSelect4},    fun tableSelect4/2},
            {{compress, tableSelect4}, fun tableSelect4/2},
            {{plain, tableUpdate},     fun tableUpdate/2},
            {{compress, tableUpdate},  fun tableUpdate/2},
            {{plain, checkThrows},     fun checkThrows/2},
            {{compress, checkThrows},  fun checkThrows/2},
            {{plain, change_user},     fun change_user/2},
            {{compress, change_user},  fun change_user/2},
            {{plain, change_owner},    fun change_owner/2},
            {{compress, change_owner}, fun change_owner/2},
            {{plain, ping_is_open},    fun ping_is_open/2},
            {{compress, ping_is_open}, fun ping_is_open/2},
            {{plain, get_field_list},    fun get_field_list/2},
            {{compress, get_field_list}, fun get_field_list/2},
            {{plain, transactionTest},    fun transactionTest/2},
            {{compress, transactionTest}, fun transactionTest/2},
            {{plain, transactionIsolationLevelTest},    fun transactionIsolationLevelTest/2},
            {{compress, transactionIsolationLevelTest}, fun transactionIsolationLevelTest/2},
            {{plain, zero_length_response},    fun zero_length_response/2},
            {{compress, zero_length_response}, fun zero_length_response/2},
            {{plain, blob},    fun blob/2},
            {{compress, blob},    fun blob/2}
          ]
        },
        {module, statement_tests},
        {module, cursor_tests},
        {module, concurrent_connection_tests}
      ]
    }
  ].

%% single_test_skip() ->
%%   [ 
%%     {"start app", fun testing:do_start/0},
%% %    {"One", fun() -> dbCreate(datasource), tableCreate(datasource) end},
%% %    {"Two", fun() -> dbCreate(datasource_compr), tableCreate(datasource_compr) end},
%% 
%%     {"Three.1", fun() -> tableSelect1(datasource), tableSelect1(datasource_compr) end},
%% 
%%     {"stop app", fun testing:do_stop/0}
%%   ]
%% .

dbCreate(DS) ->
  Connection = datasource:get_connection(DS),
  ?debug_Fmt("Connection= ~p;~n", [Connection]),
  ?assert(is_pid(Connection)),
%  ?debug_Fmt("After Connection get~n", []),
  Conn_record = connection:connection_record(Connection),
  ?debug_Fmt("Connection record= ~p; ~n", [Conn_record]),
  ?debug_Fmt("     Server = ~p; ~n", [Conn_record#connection.server_info#server_info.server_version]),
  {M,[Ok]} = connection:execute_query(Connection, "DROP DATABASE IF EXISTS eunitdb"),
  ?assert(is_record(M, metadata)),
  ?assertEqual(0, M#metadata.field_count),
  ?assertEqual(0, M#metadata.param_count),
  ?assertEqual(true, M#metadata.server_status#server_status.dbDropped),
  ?assert(is_record(Ok, ok_packet)),
  ?assertEqual(true, Ok#ok_packet.server_status#server_status.dbDropped),
%  ?debug_Fmt("~n" ++helper_common:status_to_string(Ok#ok_packet.server_status) ++ "~n", []),

  {M1,[Ok1]} = connection:execute_query(Connection, "CREATE DATABASE IF NOT EXISTS eunitdb"),
  ?assert(is_record(M1, metadata)),
  ?assertEqual(0, M1#metadata.field_count),
  ?assert(is_record(Ok1, ok_packet)),
  ?assertEqual(false, Ok1#ok_packet.server_status#server_status.dbDropped),
%  ?debug_Fmt("~n" ++helper_common:status_to_string(Ok1#ok_packet.server_status) ++ "~n", []),
  datasource:return_connection(DS, Connection).

tableCreate(C) ->
%%  ?debug_Fmt(" >>> 2. tableCreate(~p)",[C]),
%  ?debug_Fmt(" loaded apps: ~p",[application:loaded_applications()]),
  Connection = datasource:get_connection(C),
  
  Query =
  "CREATE TABLE eunitdb.all_in_one ("
   "persist_id bigint(20) NOT NULL AUTO_INCREMENT," 
   " int_col int(11) DEFAULT NULL,"
   " varchar_col varchar(45) DEFAULT NULL,"
   " decimal_34_14_col decimal(34,14) DEFAULT NULL,"
   " datetime_col datetime DEFAULT NULL,"
   " date_col date DEFAULT NULL,"
   " time_col time DEFAULT NULL,"
   " timestamp_col timestamp NULL DEFAULT NULL," 
   " year_col year(4) DEFAULT NULL,"
   " decimal_24_7_col decimal(24,7) DEFAULT NULL,"
   " double_col double DEFAULT NULL,"
   " float_col float DEFAULT NULL,"
   " mediumint_col mediumint(9) DEFAULT NULL,"
   " smallint_col smallint(6) DEFAULT NULL,"
   " usmallint_col smallint(5) unsigned DEFAULT NULL,"
   " tinyint_col tinyint(4) DEFAULT NULL,"
   " char_col char(25) DEFAULT NULL,"
   " bit_col bit(11) DEFAULT NULL,"
   " enum_col enum('one','two','three') DEFAULT NULL,"
   " set_col set('one','two','three') DEFAULT NULL,"
   " binary_col binary(250) DEFAULT NULL,"
   " blob_col blob,"
   " longblob_col longblob,"
   " mediumblob_col mediumblob,"
   " tinyblob_col tinyblob,"
   " varbinary_col varbinary(1000) DEFAULT NULL,"
   " tinytext_col tinytext,"
   " text_col text,"
   " mediumtext_col mediumtext,"
   " longtext_col longtext,"
   " PRIMARY KEY (persist_id),"
   " UNIQUE KEY persist_id_UNIQUE (persist_id)"
   " ) ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8",
  {M,[Ok]} = connection:execute_query(Connection, Query),
  ?assert(is_record(M, metadata)),
  ?assertEqual(0, M#metadata.field_count),
  ?assertEqual(0, length(M#metadata.field_metadata)),
  ?assert(is_record(Ok, ok_packet)),
  ?debug_Fmt("~n" ++helper_common:status_to_string(Ok#ok_packet.server_status) ++ "~n", []),
  datasource:return_connection(C, Connection).

tableInsert(_, C) -> fun() ->  
  Connection = datasource:get_connection(C),
  Query1 = ["INSERT INTO all_in_one VALUES" 
  , " (NULL,1025,NULL,'12034879.00000000000000','1954-05-25 22:23:16','1977-07-09',"
  , " '41:35:47','00-12-31 23:59:59',1977,'-120334765.0050500',12005.3451,"
  , " 456.33,212121,2121,717,21,'characters: char[25]',NULL,NULL,'one,two',"
  , " 'Binary(250): Binary array = AAAAAAAAAAAAAAA, BBBBBBBBBBBBBBBBBBBBB, " 
  , "JJJJJJJJJJJJJJJJJJ\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0',"
  , "'This is a BLOB object.','This is a LONG BLOB object.','This is a MEDIUM BLOB object.',"
  , "'This is a TINY BLOB object.','VARBINARY(1000): EMPTY EMPTY EMPTY.',"
  , "'This is a TINYTEXT object.',NULL,NULL,'This is a LONGTEXT object.'),"

  , "(NULL,-2510,'this is a Varchar(45): test string.','4529800123.00995672340000','1957-10-01 12:34:00','1978-06-06',"
  , "'-05:55:43','71-01-01 00:00:01',2011,'-3459123.0070077',-20988765.88,"
  , "-5.77e-006,742387,12127,1717,126,'char symbols','\07','one','three',"
  , "NULL,NULL,NULL,NULL,NULL,NULL,NULL,'This is TEXT object.','This is a MEDIUMTEXT object.',NULL),"

  , "(NULL,739245,'This is a Varchar(45) string.','4529830123.090795672340000','1957-10-01 12:34:00','1978-06-06',"
  , "'-755:55:43','71-01-01 00:00:01',2031,'-3459123.0070077',0.0,"
  , "5.0,742387,12127,17717,122,'char symbols','\07','one','three,one',"
  , "'binary column','blob field','long blob','medium blob','tiny blob','varbinary field',"
  , "'Tiny text','This is TEXT object.','This is a MEDIUMTEXT object.','Long Text Field')"
  ],
  {M, [Ok]} = connection:execute_query(Connection, Query1),
  ?assert(is_record(M, metadata)),
  ?assertEqual(0, M#metadata.field_count),
  ?assertEqual(0, length(M#metadata.field_metadata)),
  ?assert(is_record(Ok, ok_packet)),
  
  ?assertEqual(3, Ok#ok_packet.affected_rows),
%  ?debug_Fmt("~n" ++helper_common:status_to_string(Ok#ok_packet.server_status) ++ "~n", []),
  
  datasource:return_connection(C, Connection),
  ?PASSED
end.

tableSelect1(_, C) -> fun() ->  
  Connection = datasource:get_connection(C),

  {M,[R|_]} = connection:execute_query(Connection, "SELECT bit_col FROM eunitdb.all_in_one WHERE persist_id = 1"),
%  ?debug_Fmt("~p~n", [R]),
  ?assert(is_record(M, metadata)),
  ?assertEqual(1, M#metadata.field_count),
  ?assertEqual(1, length(M#metadata.field_metadata)),
  ?assert(is_list(R)),
  ?assertEqual([null], R),

  {M1,[R1|_]} = connection:execute_query(Connection, "SELECT date_col, smallint_col FROM eunitdb.all_in_one WHERE persist_id = 1"),
%  ?debug_Fmt("~p~n", [R1]),
  ?assert(is_record(M1, metadata)),
  ?assertEqual(2, M1#metadata.field_count),
  ?assertEqual(2, length(M1#metadata.field_metadata)),
  ?assert(is_list(R1)),
  ?assertEqual([#mysql_time{year=1977,month=7,day=9},2121], R1),
  
  {M3,[R2|_]} = connection:execute_query(Connection, "SELECT * FROM eunitdb.all_in_one WHERE persist_id = 1"),
  Cursor = cursor:new({M3,[R2]}),
  Val = cursor:get(Cursor, "char_col"),
  ?debug_Fmt("~n~nCURSOR ::: ~p~n", [Val]),
%  ?debug_Fmt("~p~n", [R2]),
  ?assert(is_record(M3, metadata)),
  ?assertEqual(30, M3#metadata.field_count),
  ?assertEqual(30, length(M3#metadata.field_metadata)),
  ?assert(is_list(R2)),
  Expected = [1,1025,null,{mysql_decimal,"12034879","00000000000000"},{mysql_time,false,1954,5,25,22,23,16,0},
    {mysql_time,false,1977,7,9,0,0,0,0},{mysql_time,false,0,0,0,41,35,47,0},{mysql_time,false,2000,12,31,23,59,59,0},
    1977,{mysql_decimal,"-120334765","0050500"},12005.3451,456.33,212121,2121,717,21,
    "characters: char[25]",null,null,[one,two],
    <<66,105,110,97,114,121,40,50,53,48,41,58,32,66,105,110,97,114,121,
      32,97,114,114,97,121,32,61,32,65,65,65,65,65,65,65,65,65,65,65,
      65,65,65,65,44,32,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,
      66,66,66,66,66,66,44,32,74,74,74,74,74,74,74,74,74,74,74,74,74,
      74,74,74,74,74,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0>>,
    <<"This is a BLOB object.">>,<<"This is a LONG BLOB object.">>,
    <<"This is a MEDIUM BLOB object.">>,
    <<"This is a TINY BLOB object.">>,
    <<"VARBINARY(1000): EMPTY EMPTY EMPTY.">>,
    "This is a TINYTEXT object.",null,null,
    "This is a LONGTEXT object."],  
  ?assertEqual(Expected, R2),
  
  datasource:return_connection(C, Connection),
  ?PASSED
end.

tableSelect2(_X, C) -> fun() ->  
  Connection = datasource:get_connection(C),
  
  {_, [R2 | _]} = connection:execute_query(Connection, "SELECT * FROM eunitdb.all_in_one WHERE persist_id = 2"),
%  ?debug_Fmt("~p~n", [R2]),
  ?assert(is_list(R2)),
  Expected = [2,-2510,"this is a Varchar(45): test string.",
    {mysql_decimal,"4529800123","00995672340000"},
    {mysql_time,false,1957,10,1,12,34,0,0},
    {mysql_time,false,1978,6,6,0,0,0,0},
    {mysql_time,true,0,0,0,-5,55,43,0},
    {mysql_time,false,1971,1,1,0,0,1,0},
    2011,
    {mysql_decimal,"-3459123","0070077"},
    -20988765.88,-5.77e-6,742387,12127,1717,126,"char symbols",
    <<0,7>>,one,[three],null,null,null,null,null,null,null,
    "This is TEXT object.","This is a MEDIUMTEXT object.",null
  ],  
  ?assertEqual(Expected, R2),
    
  datasource:return_connection(C, Connection),
  ?PASSED
end.

tableSelect3(_X, C) -> fun() ->  
  Connection = datasource:get_connection(C),
  
  {M,[R2|_]} = connection:execute_query(Connection, "SELECT * FROM eunitdb.all_in_one WHERE persist_id = 3"),
  ?debug_Fmt("~p~n", [R2]),
  ?assert(is_record(M, metadata)),
  ?assertEqual(30, M#metadata.field_count),
  ?assertEqual(30, length(M#metadata.field_metadata)),
  ?assert(is_list(R2)),
  Expected = [3,739245,"This is a Varchar(45) string.",
    {mysql_decimal,"4529830123","09079567234000"},
    {mysql_time,false,1957,10,1,12,34,0,0},{mysql_time,false,1978,6,6,0,0,0,0},
    {mysql_time,true,0,0,0,-755,55,43,0},{mysql_time,false,1971,1,1,0,0,1,0},
    2031,{mysql_decimal,"-3459123","0070077"},0.0,5.0,742387,
    12127,17717,122,"char symbols",<<0,7>>,one,[one,three],
    <<98,105,110,97,114,121,32,99,111,108,117,109,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
    <<"blob field">>,<<"long blob">>,<<"medium blob">>,<<"tiny blob">>,
    <<"varbinary field">>,"Tiny text","This is TEXT object.",
    "This is a MEDIUMTEXT object.","Long Text Field"
  ],  
  ?assertEqual(Expected, R2),
  
%%   lists:map(fun(#field_metadata{name = Name, flags = Flags, type = Type}) ->
%%       ?debug_Fmt("~p type: ~p flags: ~p~n", [Name,helper_common:type_to_atom(Type),helper_common:parse_data_flags(Flags)]) end, 
%%       M#metadata.field_metadata),
  datasource:return_connection(C, Connection),
  ?PASSED
end.

tableSelect4(_X, C) -> fun() ->
  Connection = datasource:get_connection(C),
  
  {_,R1} = connection:execute_query(Connection, "select * from fgamedb.t_coin_diamond_consume"),
  
  ?debug_Fmt("~p~n", [length(R1)]),
  ?assertEqual(46818, length(R1)),
  datasource:return_connection(C, Connection),
  ?PASSED
end.

tableUpdate(_X, C) -> fun() ->
  Connection = datasource:get_connection(C),
  Blob = lists:flatmap(fun(Y) -> [Y,55] end, binary_to_list(<<0:480>>)),
  {M, [R | _]} = connection:execute_query(Connection, ["UPDATE eunitdb.all_in_one SET blob_col = '"
      , Blob, "' WHERE persist_id = 1"]),
%  ?debug_Fmt("~p~n", [R]),
  ?assert(is_record(M, metadata)),
  ?assertEqual(0, M#metadata.field_count),
  ?assertEqual(0, length(M#metadata.field_metadata)),
  ?assert(is_record(R, ok_packet)),

  {M1, [R1 | _]} = connection:execute_query(Connection, "SELECT blob_col FROM eunitdb.all_in_one WHERE persist_id = 1"),
%  ?debug_Fmt("~p~n", [R1]),
  ?assert(is_record(M1, metadata)),
  ?assertEqual(1, M1#metadata.field_count),
  ?assertEqual(1, length(M1#metadata.field_metadata)),
  ?assert(is_list(R1)),
  Expected = [list_to_binary(Blob)],
  ?assertEqual(Expected, R1),

  datasource:return_connection(C, Connection),
  ?PASSED
end.

checkThrows({X, _}, _) -> {timeout, 30, fun() ->
  C = case X of plain -> 0; compress -> 1 end,
  DS_def = #datasource{
    host = "local_host", 
    port = ?TEST_SERVER_PORT, 
    database = "eunitdb", 
    user = "root", 
    password = "root", 
    flags = #client_options{compress=C}
  },
  {ok, _} = my:new_datasource(ds_1, DS_def),
  {error, Err1} = datasource:get_connection(ds_1),
  ?assertMatch(tcp, Err1#mysql_error.type),
  DS_deff = #datasource{
    host = ?TEST_SERVER_HOST_NAME, 
    port = 3300, 
    database = "eunitdb", 
    user = "root", 
    password = "root", 
    flags = #client_options{compress=C}
  },
  {ok, _} = my:new_datasource(ds_2, DS_deff),
  {error, Err2} = datasource:get_connection(ds_2),
  ?assertMatch(tcp, Err2#mysql_error.type),
  DS_defff = #datasource{
    host = ?TEST_SERVER_HOST_NAME, 
    port = ?TEST_SERVER_PORT, 
    database = "eunitdb", 
    user = "root", 
    password = "rooot", 
    flags = #client_options{compress=C}
  },
  {ok, _} = my:new_datasource(ds_3, DS_defff),
  {error, Err3} = datasource:get_connection(ds_3),
  ?assertMatch(connection, Err3#mysql_error.type),
  DS_deffff = #datasource{
    host = ?TEST_SERVER_HOST_NAME, 
    port = ?TEST_SERVER_PORT, 
    database = "eunitdb", 
    user = "rooot", 
    password = "root", 
    flags = #client_options{compress=C}
  },
  {ok, _} = my:new_datasource(ds_4, DS_deffff),
  {error, Err4} = datasource:get_connection(ds_4),
  ?assertMatch(connection, Err4#mysql_error.type),
  DS_defffff = #datasource{
    host = ?TEST_SERVER_HOST_NAME, 
    port = ?TEST_SERVER_PORT, 
    database = "eunitdb", 
    user = "root", 
    password = "root", 
    flags = #client_options{compress = C}
  },
  {ok, _} = my:new_datasource(ds_5, DS_defffff),
  Connection = datasource:get_connection(ds_5),
  ?assertMatch(sqlquery, (connection:execute_query(Connection, "SELECT * FROM eunitdbb:all_in_one"))#mysql_error.type),
  datasource:invalidate_connection(ds_5, Connection),
  ?assertEqual(connection, (connection:execute_query(Connection, "SELECT int_col FROM all_in_one WHERE persist_id = 1"))#mysql_error.type),
  datasource:close(ds_1),
  datasource:close(ds_2),
  datasource:close(ds_3),
  datasource:close(ds_4),
  datasource:close(ds_5),
  ?PASSED
end}.

change_user(_X, C) -> fun() ->
  Connection = datasource:get_connection(C),
%%  ?debug_Fmt("~n~n ++++++++++ change_user +++++++++++ ~p~n ", [connection:connection_record(Connection)]),
  {_,[R|_]} = connection:execute_query(Connection, "SELECT date_col, smallint_col FROM all_in_one WHERE persist_id = 1"),
  ?assert(is_list(R)),
  ?assertEqual([#mysql_time{year=1977,month=7,day=9},2121], R),
  ok = datasource:change_user(C, "user", "user", "eunitdb"),
  {_,[R1|_]} = connection:execute_query(Connection, "SELECT date_col, smallint_col FROM eunitdb.all_in_one WHERE persist_id = 1"),
%%  ?debug_Fmt("~n~n ++++++++++ change_user +++++++++++ ~p~n ", [connection:connection_record(Connection)]),
  ?assert(is_list(R1)),
  ?assertEqual("user", (connection:connection_record(Connection))#connection.ds_def#datasource.user),
  ?assertEqual([#mysql_time{year=1977,month=7,day=9},2121], R1),
  datasource:return_connection(C, Connection),
  ?PASSED
end.

change_owner(_X, C) -> fun() ->
  Connection = datasource:get_connection(C),
%%  ?debug_Fmt("~n~n ++++++++++ change_owner +++++++++++ ~p~n ", [connection:connection_record(Connection)]),
  ok = gen_server:call(Connection, {change_owner, null}),
%%  ?debug_Fmt("~n~n ++++++++++ change_owner +++++++++++ ~p~n ", [connection:connection_record(Connection)]),
  datasource:return_connection(C, Connection),
%%  ?debug_Fmt("~n~n ++++++++++ change_owner +++++++++++ ~p~n ", [connection:connection_record(Connection)])
  ?PASSED
end.

ping_is_open(_X, C) -> fun() ->
  Connection = datasource:get_connection(C),
  Is_open = connection:is_open(Connection),
%%  ?debug_Fmt("is_open response = ~p; ~n", [Is_open]),
  ?assertEqual(Is_open, true),
  gen_server:cast(Connection, break_connection),
  Is_open_1 = connection:is_open(Connection),
%%  ?debug_Fmt("is_open response = ~p; ~n", [Is_open_1]),
  ?assertEqual(Is_open_1, false),
  datasource:return_connection(C, Connection),
  ?PASSED
end.
  
get_field_list(_X, C) -> fun() ->
  Connection = datasource:get_connection(C),
  {#metadata{field_metadata = M},[]} = connection:get_field_list(Connection, "all_in_one", "%"), 
%  ?debug_Fmt("Field List = ~p; ~n", [M]),
  [R | _] = M,
  ?assert(is_record(R, field_metadata)),
  ?assertEqual(30, length(M)),

  {#metadata{field_metadata = M1},[]} = connection:get_field_list(Connection, "all_in_one", "varchar_col"), 
  [R1 | _] = M1,
  ?assert(is_record(R1, field_metadata)),
  ?assertEqual(1, length(M1)),
  
  datasource:return_connection(C, Connection),
  ?PASSED
end.

transactionTest(_X, Cm) -> fun() ->
  Connection = datasource:get_connection(Cm),
  {_, [[R | _] | _]} = connection:execute_query(Connection, "SELECT smallint_col FROM all_in_one WHERE persist_id = 1"),
%  ?debug_Fmt("Field = ~p; ~n", [R]),
  Inc_2 = fun(Con, X) ->
    connection:execute_query(Con, lists:concat(["UPDATE eunitdb.all_in_one SET smallint_col = smallint_col + 2",
                                         " WHERE persist_id = 1"])),
    connection:execute_query(Con, "SEL" ++ X ++ "ECT smallint_col FROM all_in_one WHERE persist_id = 1")
  end,

  connection:transaction(Connection, fun (C) -> Inc_2(C, "") end),

  {_, [[Rr | _] | _]} = connection:execute_query(Connection, "SELECT smallint_col FROM all_in_one WHERE persist_id = 1"),
%  ?debug_Fmt("Field = ~p; ~n", [Rr]),
  ?assertEqual(R + 2, Rr),

  case TR = connection:transaction(Connection, fun (C) -> Inc_2(C, " ") end) of
    #mysql_error{} -> ?debug_Fmt("     Exception 1 = ~p; ~n", [TR]);
    _ -> ok
  end,
  {_, [[Re | _] | _]} = connection:execute_query(Connection, "SELECT smallint_col FROM all_in_one WHERE persist_id = 1"),
%  ?debug_Fmt("Field = ~p; ~n", [Re]),
  ?assertEqual(R + 2, Re),

  case TR1 = connection:transaction(Connection, fun (C) -> Inc_2(C, ""), C = 1 end) of
    #mysql_error{}  -> ?debug_Fmt("     Exception 2 = ~p; ~n", [TR1]);
    _ -> ok
  end,
  {_, [[Ree | _] | _]} = connection:execute_query(Connection, "SELECT smallint_col FROM all_in_one WHERE persist_id = 1"),
%  ?debug_Fmt("Field = ~p; ~n", [Ree]),
  ?assertEqual(R + 2, Ree),

  datasource:return_connection(Cm, Connection),
  ?PASSED
end.

transactionIsolationLevelTest({X, _}, _) -> fun() ->
  Cm = case X of plain -> 0; compress -> 1 end,
  Client_flags = #client_options{compress = Cm, trans_isolation_level = read_uncommitted},
  DS_def = #datasource{
    host = ?TEST_SERVER_HOST_NAME, 
    port = ?TEST_SERVER_PORT, 
    database = "eunitdb", 
    user = "root", 
    password = "root", 
    flags = Client_flags
  },
  {ok, _} = my:new_datasource(test, DS_def),
  Conn = datasource:get_connection(test),
  
  Transaction_fun = fun(Con) ->
    connection:execute_query(Con, "UPDATE eunitdb.all_in_one SET smallint_col = smallint_col + 2"
     " WHERE persist_id = 1"),
    connection:execute_query(Con, "SELECT smallint_col FROM all_in_one WHERE persist_id = 1")
  end,
  
  {_, [[Re | _] | _]} = connection:transaction(Conn, Transaction_fun),
  ?debug_Fmt(" >>> 10.2 Ree = ~p",[Re]),
  
  datasource:return_connection(test, Conn),
  datasource:close(test),
%% ------------------------------------------
  {ok, _} = my:new_datasource(test, DS_def#datasource{flags = Client_flags#client_options{trans_isolation_level = read_committed}}),
  Conn2 = datasource:get_connection(test),
  
  {_, [[Ree | _] | _]} = connection:transaction(Conn2, Transaction_fun),
  ?debug_Fmt(" >>> 10.2 Ree = ~p",[Ree]),
  
  datasource:return_connection(test, Conn2),
  datasource:close(test),
%% ------------------------------------------
  {ok, _} = my:new_datasource(test, DS_def#datasource{flags = Client_flags#client_options{trans_isolation_level = serializable}}),
  Conn3 = datasource:get_connection(test),
  
  {_, [[Reee | _] | _]} = connection:transaction(Conn3, Transaction_fun),
  ?debug_Fmt(" >>> 10.2 Ree = ~p",[Reee]),
  
  datasource:return_connection(test, Conn3),
  datasource:close(test),
%% ------------------------------------------
  {ok, _} = my:new_datasource(test, DS_def#datasource{flags = Client_flags#client_options{trans_isolation_level = repeatable_read}}),
  Conn4 = datasource:get_connection(test),
  
  {_, [[Reeee | _] | _]} = connection:transaction(Conn4, Transaction_fun),
  ?debug_Fmt(" >>> 10.2 Ree = ~p",[Reeee]),
  
  datasource:return_connection(test, Conn4),
  datasource:close(test),
  ?PASSED
end.

zero_length_response(_X, Cm) -> fun() ->
  Connection = datasource:get_connection(Cm),
  Query = "UPDATE all_in_one SET longtext_col= ? WHERE persist_id = ?;",
  Handle = connection:get_prepared_statement_handle(Connection, Query),

  {_,[R|_]} = connection:execute_statement(Connection, Handle, [?LONG_BLOB, ?LONGLONG], ["", 1]),
  ?assert(is_record(R, ok_packet)),

  connection:close_statement(Connection, Handle),

  Query1 = "SELECT longtext_col FROM all_in_one WHERE persist_id=",
  {_,[R1|_]} = connection:execute_query(Connection, Query1 ++ "1"),
  ?assert(is_list(R1)),
  [Field_1 | _] = R1,
%  ?debug_Fmt(".~p.~n", [Field_1]),

  ?assertEqual(0, length(Field_1)),
  
  Query2 = "SELECT longtext_col FROM all_in_one WHERE persist_id= ?;",
  Handle2 = connection:get_prepared_statement_handle(Connection, Query2),

  {_,[R2|_]} = connection:execute_statement(Connection, Handle2, [?LONGLONG], [1]),
  ?assert(is_list(R2)),
  [Field_2 | _] = R2,
%  ?debug_Fmt(".~p.~n", [Field_2]),

  ?assertEqual(0, length(Field_2)),

  connection:close_statement(Connection, Handle2),

  datasource:return_connection(Cm, Connection),
  ?PASSED
end.

for(0, _) -> ok;
for(N, F) -> F(N), for(N-1, F).

for_bin(1, F) -> F(1);
for_bin(N, F) -> list_to_binary([F(N) | for_bin(N-1, F)]).

blob() -> 
  ?debug_Msg(" >>> blob()"),
  L = 16#100000 - 64, % 16#100000 - max alowed packet size
  for_bin(L div 64, fun(M) -> <<M:512/integer>> end).

blob(_X, C) -> {timeout, 100, fun() ->
  Connection = datasource:get_connection(C),
  Query = "INSERT INTO all_in_one (longblob_col) VALUES (?)",
  Handle = connection:get_prepared_statement_handle(Connection, Query),
  B = blob(),
  L = size(B),
  K = 1,

  ?debug_Fmt(" >>> send long parameter ~p bytes size.",[L*K]),
  for(K, fun(_M) -> connection:send_statement_long_parameter(Connection, Handle, 0, B) end),

  {_,[R|_]} = connection:execute_statement(Connection, Handle, [?LONG_BLOB], [null]),
  ?assert(is_record(R, ok_packet)),
  Id = R#ok_packet.insert_id,
%  ?debug_Fmt("L: ~p Id: ~p~n", [L*K, Id]),
  connection:close_statement(Connection, Handle),

  Query1 = ["SELECT longblob_col FROM all_in_one WHERE persist_id=", integer_to_list(Id)],
  {_,[R1|_]} = connection:execute_query(Connection, Query1),
  [Field_1 | _] = R1, 
  ?assertEqual(L*K, size(Field_1)),
%  ?debug_Fmt("L: ~p ~n", [size(Field_1)]),

  Query2 = "SELECT longblob_col FROM all_in_one WHERE persist_id=?",
  Handle2 = connection:get_prepared_statement_handle(Connection, Query2),
  {_,[R2|_]} = connection:execute_statement(Connection, Handle2, [?LONGLONG], [Id]),
  [Field_2 | _] = R2,
  ?assertEqual(L*K, size(Field_2)),
%  ?debug_Fmt("L: ~p ~n", [size(Field_2)]),

  connection:close_statement(Connection, Handle2),
  datasource:return_connection(C, Connection),
  ?PASSED
end}.
