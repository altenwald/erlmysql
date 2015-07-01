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

%% @since 2010-12-21
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module contains functions these are responsible for processing prepared statements.

-module(helper_statement).

%%
%% Include files
%%
-include("client_records.hrl").
-include("mysql_types.hrl").

%%
%% Import modules
%%

%%
%% Exported Functions
%%
-export([pack_types/1, pack_parameters/2, pack_null_bit_map/2]).

%% @spec pack_types(Types::list(mysql_type())) -> binary() 
%% mysql_type() = integer()
%% 
%% @doc The function converts list of constants representing a MySQL types of prepared statement parameters to binary(). 
%% The binary will be inserted to prepared statement execution command.
%%
pack_types(Types) -> pack_types(Types, <<>>).

%% @spec pack_types(Parameter_types, Result::binary()) -> binary()
%% where
%% Parameter_types = list(integer())
%% 
%% @doc Converts list of parameter types to binary.
%%
pack_types([], B) -> B;
pack_types([Type|Rest], B) ->
  pack_types(Rest, <<B/binary, Type:16/little-integer>>) .

%% @spec pack_parameters(Types, Params) -> binary()
%% where
%% Types = list(integer())
%% Params = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%%
%% @doc The function converts list of parameters to binary that is prepared to form statement execute command.
%%
pack_parameters(Types, Params) -> pack_parameters(Types, Params, <<>>, 1).

%% @spec pack_parameters(Parameter_types, Parameters, Result::binary(), Index::integer()) -> binary()
%% where
%% Parameter_types = list(integer())
%% Parameters = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%% 
%% @doc Converts list of parameter types to binary.
%%
pack_parameters(_, [], B, _) -> B;
pack_parameters([Type|Trest], [Param|Prest], B, Idx) -> 
  case R = check_param(Type, Param, Idx) of
    #mysql_error{} -> pack_parameters([], [], R, 0);
    true ->
      pack_parameters(Trest, Prest, <<B/binary, (param_to_binary(Type, Param))/binary>>, Idx + 1)
  end.

%% @spec pack_null_bit_map(Parameter_types, Parameters) -> binary()
%% where
%% Parameter_types = list(integer())
%% Parameters = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%%
%% @doc Generates binary that represents null bit map - definition of parameters with NULL value.
%%
pack_null_bit_map(Parameter_types, Parameters) -> 
  L = length(Parameters),
  BitmapSize = ((L + 7) div 8) * 8,
  <<(null_bit_map(Parameter_types, Parameters, 1, 0)):BitmapSize/integer-little>>.

%% @spec null_bit_map(Parameter_types, Parameters, Mask::integer(), N::integer()) -> integer()
%% where
%% Parameter_types = list(integer())
%% Parameters = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%% 
%% @doc Converts list of parameter types and list of parameter to null bit map structure.
%%
null_bit_map([], [], _Mask, N) -> N;
null_bit_map([?MYSQL_TYPE_NULL | Rest_types], [_Param | Rest_parameters], Mask, N) ->
  null_bit_map(Rest_types, Rest_parameters, Mask bsl 1, N bor Mask);
null_bit_map([_Type | Rest_types], [null | Rest_parameters], Mask, N) ->
  null_bit_map(Rest_types, Rest_parameters, Mask bsl 1, N bor Mask);
null_bit_map([_Type | Rest_types], [_Param | Rest_parameters], Mask, N) ->
  null_bit_map(Rest_types, Rest_parameters, Mask bsl 1, N).

%% @spec param_to_binary(Parameter_type, Parameter) -> binary()
%% where
%% Parameter_type = integer()
%% Parameter = integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{}
%% 
%% @doc Converts parameter value to binary.
%%
param_to_binary(_, null) -> <<>>;
param_to_binary(?MYSQL_TYPE_NULL, _) -> <<>>;
param_to_binary(?MYSQL_TYPE_TINY, P) -> <<P:8/signed-little-integer>>;
param_to_binary(?MYSQL_TYPE_TINY + ?UNSIGNED, P) -> <<P:8/little-integer>>;
param_to_binary(?MYSQL_TYPE_SHORT, P) -> <<P:16/little-integer>>;
param_to_binary(?MYSQL_TYPE_SHORT + ?UNSIGNED, P) -> <<P:16/signed-little-integer>>;
param_to_binary(?MYSQL_TYPE_INT24, P) ->helper_common:string_to_length_coded_binary(erlang:integer_to_list(P, 10));
param_to_binary(?MYSQL_TYPE_INT24 + ?UNSIGNED, P) ->helper_common:string_to_length_coded_binary(erlang:integer_to_list(P, 10));
param_to_binary(?MYSQL_TYPE_LONG, P) -> <<P:32/signed-little-integer>>;
param_to_binary(?MYSQL_TYPE_LONG + ?UNSIGNED, P) -> <<P:32/little-integer>>;
param_to_binary(?MYSQL_TYPE_LONGLONG, P) -> <<P:64/signed-little-integer>>;
param_to_binary(?MYSQL_TYPE_LONGLONG + ?UNSIGNED, P) -> <<P:64/little-integer>>;
param_to_binary(?MYSQL_TYPE_FLOAT, P) -> <<P:32/little-float>>;
param_to_binary(?MYSQL_TYPE_DOUBLE, P) -> <<P:64/little-float>>;
param_to_binary(?MYSQL_TYPE_TIME, P) ->helper_common:time_to_lc_binary(P);
param_to_binary(?MYSQL_TYPE_TIMESTAMP, P) ->helper_common:datetime_to_lc_binary(P);
param_to_binary(?MYSQL_TYPE_DATETIME, P) ->helper_common:datetime_to_lc_binary(P);
param_to_binary(?MYSQL_TYPE_DATE, P) ->helper_common:date_to_lc_binary(P);
param_to_binary(?MYSQL_TYPE_YEAR, P) ->helper_common:string_to_length_coded_binary(erlang:integer_to_list(P, 10));
param_to_binary(?MYSQL_TYPE_VARCHAR, P) ->helper_common:string_to_length_coded_binary(P);
param_to_binary(T, P) when T =:= ?MYSQL_TYPE_VAR_STRING; T =:= ?MYSQL_TYPE_STRING ->
  if
    is_binary(P) -> helper_common:binary_to_length_coded_binary(P);
    true ->  helper_common:string_to_length_coded_binary(P)
  end;
param_to_binary(?MYSQL_TYPE_ENUM, P) ->helper_common:string_to_length_coded_binary(atom_to_list(P));
param_to_binary(?MYSQL_TYPE_SET, P) ->helper_common:string_to_length_coded_binary(helper_common:atom_list_to_string(P));
param_to_binary(T, P) when T =:= ?MYSQL_TYPE_BLOB; T =:= ?MYSQL_TYPE_LONG_BLOB; 
                           T =:= ?MYSQL_TYPE_MEDIUM_BLOB; T =:= ?MYSQL_TYPE_TINY_BLOB -> 
  if
    is_binary(P) -> helper_common:binary_to_length_coded_binary(P);
    true ->  helper_common:string_to_length_coded_binary(P)
  end;
param_to_binary(?MYSQL_TYPE_DECIMAL, P) ->helper_common:decimal_to_lc_binary(P);
param_to_binary(?MYSQL_TYPE_NEWDECIMAL, P) ->helper_common:decimal_to_lc_binary(P);
param_to_binary(?MYSQL_TYPE_BIT, P) -> 
  BitSize = bit_size(P),
  Size = ((BitSize + 7) div 8) * 8 - BitSize,
  helper_common:binary_to_length_coded_binary(<<0:Size, P:BitSize/bitstring>>);
param_to_binary(_, _) -> <<>>.

%% @spec check_param(Parameter_type, Parameter, Index) -> true | #mysql_error{}
%% where
%% Parameter_type = integer()
%% Parameter = integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{}
%% Index = integer()
%% 
%% @doc Checks parameter value and throws exception if value is invalid.
%%
check_param(_, null, _) -> true;
check_param(?MYSQL_TYPE_NULL, _, _) -> true;
check_param(T, P, I) when (T >= ?MYSQL_TYPE_UNSIGNED)  and (not is_integer(P)) -> 
  check_error(I, " is not integer.");
check_param(T, P, I) when (T >= ?MYSQL_TYPE_UNSIGNED)  and (P < 0) -> 
  check_error(I, " has unsigned type but negative.");
check_param(T, P, I) when (T =:= ?MYSQL_TYPE_TINY orelse T =:= ?MYSQL_TYPE_SHORT orelse T =:= ?MYSQL_TYPE_INT24 
  orelse T =:= ?MYSQL_TYPE_LONG orelse T =:= ?MYSQL_TYPE_LONGLONG)  and (not is_integer(P)) -> 
  check_error(I, " is not integer.");
check_param(?MYSQL_TYPE_TINY, P, I) when P > 16#7F orelse P < -16#80 -> 
  check_error(I, " of TINY type is not in [127,-128]."); 
check_param(?MYSQL_TYPE_TINY + ?MYSQL_TYPE_UNSIGNED, P, I) when P > 16#FF orelse P < 0 -> 
  check_error(I, " of UNSIGNED TINY type is not in [0,255]."); 
check_param(?MYSQL_TYPE_SHORT, P, I) when P > 16#7FFF orelse P < -16#8000 ->
  check_error(I, " of SHORT type is not in [32767,-32768].");
check_param(?MYSQL_TYPE_SHORT + ?MYSQL_TYPE_UNSIGNED, P, I) when P > 16#FFFF orelse P < 0 ->
  check_error(I, " of UNSIGNED SHORT type is not in [0,65535].");
check_param(?MYSQL_TYPE_INT24, P, I) when P > 16#7FFFFF orelse P < -16#800000 ->
  check_error(I, " of INT24 type is not in [8388607,-8388608].");
check_param(?MYSQL_TYPE_INT24 + ?MYSQL_TYPE_UNSIGNED, P, I) when P > 16#FFFFFF orelse P < 0 ->
  check_error(I, " of UNSIGNED INT24 type is not in [0,16777215].");
check_param(?MYSQL_TYPE_LONG, P, I) when P > 16#7FFFFFFF orelse P < -16#80000000 ->
  check_error(I, " of LONG type is not in [2147483647,-2147483648].");
check_param(?MYSQL_TYPE_LONG + ?MYSQL_TYPE_UNSIGNED, P, I) when P > 16#FFFFFFFF orelse P < 0 ->
  check_error(I, " of UNSIGNED LONG type is not in [0,4294967295].");
check_param(?MYSQL_TYPE_LONGLONG, P, I) when P > 16#7FFFFFFFFFFFFFFF orelse P < -16#8000000000000000 ->
  check_error(I, " of LONGLONG type is not in [9223372036854775807,-9223372036854775808].");
check_param(?MYSQL_TYPE_LONGLONG + ?MYSQL_TYPE_UNSIGNED, P, I) when P > 16#FFFFFFFFFFFFFFFF orelse P < 0 ->
  check_error(I, " of UNSIGNED LONGLONG type is not in [0,18446744073709551615].");
check_param(?MYSQL_TYPE_FLOAT, P, I) when not is_float(P) ->
  check_error(I, " is not float.");
check_param(?MYSQL_TYPE_DOUBLE, P, I) when not is_float(P) ->
  check_error(I, " is not float.");
check_param(?MYSQL_TYPE_TIME, P, I) when not is_record(P, mysql_time) -> 
  check_error(I, " is not #mysql_time record.");
check_param(?MYSQL_TYPE_TIMESTAMP, P, I) when not is_record(P, mysql_time) ->
  check_error(I, " is not #mysql_time record.");
check_param(?MYSQL_TYPE_DATETIME, P, I) when not is_record(P, mysql_time) ->
  check_error(I, " is not #mysql_time record.");
check_param(?MYSQL_TYPE_DATE, P, I) when not is_record(P, mysql_time) ->
  check_error(I, " is not #mysql_time record.");
check_param(?MYSQL_TYPE_YEAR, P, I) when not is_integer(P) ->
  check_error(I, " is not integer.");
check_param(?MYSQL_TYPE_VARCHAR, P, I) when not is_list(P) ->
  check_error(I, " is not string.");
check_param(?MYSQL_TYPE_ENUM, P, I) when not is_atom(P) ->
  check_error(I, " is not atom.");
check_param(?MYSQL_TYPE_SET, P, I) when not is_list(P) ->
  check_error(I, " is not list.");
check_param(?MYSQL_TYPE_DECIMAL, P, I) when not is_record(P, mysql_decimal) ->
  check_error(I, " is not #mysql_decimal record.");
check_param(?MYSQL_TYPE_NEWDECIMAL, P, I) when not is_record(P, mysql_decimal) ->
  check_error(I, " is not #mysql_decimal record.");
check_param(?MYSQL_TYPE_BIT, P, I) when not is_bitstring(P) -> 
  check_error(I, " is not bitstring.");
check_param(T, P, I) when (T =:= ?MYSQL_TYPE_BLOB orelse T =:= ?MYSQL_TYPE_LONG_BLOB orelse
  T =:= ?MYSQL_TYPE_MEDIUM_BLOB orelse T =:= ?MYSQL_TYPE_TINY_BLOB)  
  and (not (is_binary(P) or is_list(P))) -> 
  check_error(I, " is not binary or string.");
check_param(_, _P, _I) -> true.

check_error(I, Message) ->
  #mysql_error{type = statement, message = "Parameter #" ++ integer_to_list(I) ++ Message, 
                source = "helper_statement:check_param/3"}.

