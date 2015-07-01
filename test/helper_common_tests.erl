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
%% @since 2011-05-04
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc This module is running unit tests for helper_common module.

-module(helper_common_tests).

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
-import(helper_common, []).

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

helper_common_test_() ->
	[ 
		{"helper_common 1", fun reverse_binary_testing/0},
		{"helper_common 2", fun parse_data_flags_testing/0},
		{"helper_common 3", fun extract_length_coded_integer_testing/0},
		{"helper_common 4", fun binary_to_datetime_testing/0},
		{"helper_common 5", fun extract_length_coded_binary_testing/0},
		{"helper_common 6", fun extract_length_coded_string_testing/0},
		{"helper_common 7", fun integer_to_length_coded_binary_testing/0},
		{"helper_common 8", fun binary_to_length_coded_binary_testing/0},
		{"helper_common 9", fun string_to_length_coded_binary_testing/0},
		{"helper_common 10", fun decimal_to_lc_binary_testing/0},
		{"helper_common 11", fun datetime_to_lc_binary_testing/0}
	].

reverse_binary_testing() ->
	?debug_Msg(">>> reverse_binary_test()"),
	?assertEqual(<<7,6,5,4,3,2,1>>, helper_common:reverse_binary(<<1,2,3,4,5,6,7>>)).

parse_data_flags_testing() ->
	?debug_Msg(">>> parse_data_flags_testing()"),
	?assert(helper_common:parse_data_flags(<<16#80:8, 16#1:8>>) =:= [binary, enum]),
	?assert(helper_common:parse_data_flags(<<16#1:8, 16#80:8>>) =:= [not_null, num]).

extract_length_coded_integer_testing() ->
	?debug_Msg(">>> extract_length_coded_integer_test()"),
	?assert(helper_common:extract_length_coded_integer(<<77:8, 1:80>>) =:= {77, <<1:80>>}),
	?assert(helper_common:extract_length_coded_integer(<<250:8, 1:80>>) =:= {250, <<1:80>>}),
	?assert(helper_common:extract_length_coded_integer(<<251:8, 1:80>>) =:= {null, <<1:80>>}),
	?assert(helper_common:extract_length_coded_integer(<<252:8, 77:16/little-integer, 1:80>>) =:= {77, <<1:80>>}),
	?assert(helper_common:extract_length_coded_integer(<<253:8, 77:24/little-integer, 1:80>>) =:= {77, <<1:80>>}),
	?assert(helper_common:extract_length_coded_integer(<<254:8, 77:64/little-integer, 1:80>>) =:= {77, <<1:80>>}),
	?assert(helper_common:extract_length_coded_integer(<<255:8, 1:80>>) =:= {0, <<1:80>>}).

extract_length_coded_binary_testing() ->
	?debug_Msg(">>> extract_length_coded_binary_testing()"),
	?assert(helper_common:extract_length_coded(true, <<7:8, 0:56, 77:80>>) =:= {<<0:56>>, <<77:80>>}).

extract_length_coded_string_testing() ->
	?debug_Msg(">>> extract_length_coded_string_testing()"),
	?assert(helper_common:extract_length_coded(false, <<11:8, "test string", 77:80>>) =:= {"test string", <<77:80>>}).

binary_to_datetime_testing() ->
	?debug_Msg(">>> binary_to_datetime_test()"),
	?assertEqual({#mysql_time{year=1945,month=5,day=9,hour=12,minute=33,second=57,second_part=7},<<>>},
			helper_common:binary_to_datetime(<<11:8, 1945:16/little-integer, 5:8, 9:8, 12:8, 33:8, 57:8, 7:32/little-integer>>)),
	?assertEqual({#mysql_time{year=1954,month=5,day=25,hour=0,minute=0,second=0,second_part=0},<<>>},
			helper_common:binary_to_datetime(<<4:8, 1954:16/little-integer, 5:8, 25:8>>)),
	?assertEqual({#mysql_time{year=1957,month=10,day=1,hour=23,minute=59,second=4,second_part=0},<<>>},
			helper_common:binary_to_datetime(<<7:8, 1957:16/little-integer, 10:8, 1:8, 23:8, 59:8, 4:8>>)).

integer_to_length_coded_binary_testing() ->
	?debug_Msg(">>> integer_to_length_coded_binary_testing"),
	?assert(helper_common:integer_to_length_coded_binary(77) =:= <<77:8>>),
	?assert(helper_common:integer_to_length_coded_binary(12753) =:= <<252:8, 12753:16/little-integer>>),
	?assert(helper_common:integer_to_length_coded_binary(128345) =:= <<253:8, 128345:24/little-integer>>),
	?assert(helper_common:integer_to_length_coded_binary(22378945) =:= <<254:8, 22378945:64/little-integer>>),
	?assert(helper_common:integer_to_length_coded_binary(2237894500000000000000) =:= 
            #mysql_error{type = statement, source="helper_common:integer_to_length_coded_binary/1", message = "Parameter is too long."}).

binary_to_length_coded_binary_testing() ->
	?debug_Msg(">>> binary_to_length_coded_binary_testing"),
	?assert(helper_common:binary_to_length_coded_binary(<<22:40, 33:32, 77:24>>) 
		=:= <<12:8, 22:40, 33:32, 77:24>>).

string_to_length_coded_binary_testing() ->
	?debug_Msg(">>> string_to_length_coded_binary_testing"),
	?assert(helper_common:string_to_length_coded_binary("test string") 
		=:= <<11:8, "test string">>).

decimal_to_lc_binary_testing() ->
	?debug_Msg(">>> decimal_to_lc_binary_testing"),
	?assert(helper_common:decimal_to_lc_binary(#mysql_decimal{int = "101", fraction = "202"}) =:= <<7:8, "101.202">>).

datetime_to_lc_binary_testing() ->
	?debug_Msg(">>> datetime_to_lc_binary_testing"),
	?assertEqual(<<11:8, 2011:16/little-integer, 5:8, 4:8, 15:8, 25:8, 45:8, 22:32/little-integer>>,
		helper_common:datetime_to_lc_binary(#mysql_time{year = 2011, month = 5, day = 4, 
						hour = 15, minute = 25, second = 45, second_part = 22})
		),
	?assertEqual(<<7:8, 2011:16/little-integer, 5:8, 4:8, 15:8, 25:8, 45:8>>,
		helper_common:datetime_to_lc_binary(#mysql_time{year = 2011, month = 5, day = 4, 
						hour = 15, minute = 25, second = 45})
		).

