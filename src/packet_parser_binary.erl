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

%% @since 2010-11-18
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc This module contains functions these are responsible for parsing binary rows that was previously 
%% exctructed from response packages. The module works only with binary rows these usualy come in to 
%% a client during a statement execution.

-module(packet_parser_binary).

%%
%% Include files
%%
-include("client_records.hrl").
-include("mysql_types.hrl").

%%
%% Import modules
%%
-import(lists, [reverse/1]).
-import(helper_common, []).

%%
%% Exported Functions
%%
-export([parse_binary_row/2]).

%%
%% API Functions
%%
-spec parse_binary_row(Binary::binary(), Metadata::#metadata{}) -> list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{}) | #mysql_error{}.

%% @spec parse_binary_row(Binary::binary(), Metadata::#metadata{}) -> list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{}) | #mysql_error{}
%% @doc function converts binary representation of a packet to list of field values. 
%% First it extracts 'Null bit map' structure from binary. The map keeps information 
%% about what fields of the response is NULL
%% (if a field is NULL the one is not present in the next part of the binary packet).
%%
parse_binary_row(B, #metadata{field_count = N} = Metadata) -> 
  Bitmap_size = ((N + 9) div 8),
  <<Null_bitmap:Bitmap_size/bytes, B1/binary>> = B,
  Bit_size = Bitmap_size * 8 - 2,
  <<NULL_bit_map:Bit_size/bits, _:2>> = helper_common:reverse_binary(Null_bitmap),
  case Data = loop_binary_row(B1, Metadata#metadata.field_metadata, NULL_bit_map, []) of
    #mysql_error{} -> Data;
    _ -> Data
  end.

%%
%% Local Functions
%%
-spec loop_binary_row(Binary::binary(), Field_metadata_list::list(), NULL_bit_map::binary(), ValueList::list()) -> list() | #mysql_error{}.

%% @spec loop_binary_row(Binary::binary(), Field_metadata_list::list(), NULL_bit_map::binary(), ValueList::list()) -> list() | #mysql_error{}
%% @doc Converts Binary (row of query result set) to list of field values according by field metadata information stored in Field_metadata_list and NULL_bit_map.
%% Returns list of formatted field values. Note that processing of NULL fields is skiped. 
loop_binary_row(Empty, [], _, Result) -> 
  if
    size(Empty) =:= 0 -> reverse(Result);
    true ->
      #mysql_error{type = connection, message = "Packet format is wrong",
              source = "loop_binary_row(E, [], _, Result)"}
  end;
loop_binary_row(B, [Field_metadata | Metadata], NULL_bit_map_param, Result) ->
  Bit_map_size = bit_size(NULL_bit_map_param) - 1,
  <<NULL_bit_map:Bit_map_size/bits, Bit:1/integer>> = NULL_bit_map_param,
  if 
    Bit =:= 1 -> 
      loop_binary_row(B, Metadata, NULL_bit_map, [null | Result]);
    true ->
      Flags = helper_common:parse_data_flags(Field_metadata#field_metadata.flags),
      Is_binary = lists:member(binary, Flags),
      Is_enum = lists:member(enum, Flags),
      Is_set = lists:member(set, Flags),
      Is_unsigned = lists:member(unsigned, Flags),
      {Value, B1} = draw_value(B, Field_metadata, {Is_binary, Is_enum, Is_set, Is_unsigned}),
      loop_binary_row(B1, Metadata, NULL_bit_map, [Value | Result])
  end.

%% @spec draw_value(B::binary, Field_metadata::#field_metadata{}, Flags::list()) -> {Value, Rest::binary()} 
%% @throws nothing
%% @doc Extracts a field value from binary stream. Returns tuple of the field value and a rest of binary stream.
%%
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_FLOAT}, _Flags) ->
  <<Value:32/little-float, B1/binary>> = B,
  {Value, B1};
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_DOUBLE}, _Flags) ->
  <<Value:64/little-float, B1/binary>> = B,
  {Value, B1};
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_TIMESTAMP}, _Flags) ->
  helper_common:binary_to_datetime(B);
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_DATE}, _Flags) ->
  <<Field_size:8/integer, V0:Field_size/binary, B1/binary>> = B,
  case V0 of
    <<>> ->
      {#mysql_time{year = 0, month = 0, day = 0}, B1};
    _ -> 
      <<Year:16/little-integer, Month:8/integer, Day:8/integer>> = V0,
      {#mysql_time{year = Year, month = Month, day = Day}, B1}
  end;
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_TIME}, _Flags) ->
  <<Field_size:8/integer, V0:Field_size/binary, B1/binary>> = B,
  <<Negative:8/integer, Day:32/little-integer, 
    Hour:8/integer, Minute:8/integer, Second:8/integer, Sec_part/binary>> = V0,
  Second_part =
  case Sec_part of 
    <<>> -> 0;  
    <<Sec_part_int:32/little-integer>> -> Sec_part_int
  end,
  {#mysql_time{neg = (Negative =/= 0), hour = Hour + Day * 24, minute = Minute, 
              second = Second, second_part = Second_part}, B1};
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_DATETIME}, _Flags) ->
  helper_common:binary_to_datetime(B);
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_YEAR}, _Flags) ->
  <<Value:16/little-integer, B1/binary>> = B,
  {Value, B1};
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_VARCHAR}, _Flags) ->
  helper_common:extract_length_coded(false, B);
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_BIT, length = Value_size}, _Flags) ->
  <<Field_size:8/integer, V:Field_size/binary, B1/binary>> = B,
  BitSize = Field_size * 8 - Value_size,
  <<0:BitSize, Value:Value_size/bitstring>> = V,
  {Value, B1};
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_NEWDECIMAL}, _Flags) ->
  {String, B1} = helper_common:extract_length_coded(false, B),
  case string:tokens(String, ".") of
    [Int, Fract] -> {#mysql_decimal{int = Int, fraction = Fract}, B1};
    [Int] -> {#mysql_decimal{int = Int}, B1};
    _ ->  {#mysql_decimal{}, B1}
  end;
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_BLOB}, {Is_binary, _, _, _}) ->
  helper_common:extract_length_coded(Is_binary, B);
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_VAR_STRING}, {Is_binary, _, _, _}) ->
  helper_common:extract_length_coded(Is_binary, B);
draw_value(B, #field_metadata{type = ?MYSQL_TYPE_STRING}, {Is_binary, Is_enum, Is_set, _}) ->
  if
    Is_binary -> helper_common:extract_length_coded(true, B);
    Is_enum -> 
      {String, B1} = helper_common:extract_length_coded(false, B),
      {list_to_atom(String), B1};
    Is_set -> 
      {String, B1} = helper_common:extract_length_coded(false, B),
      {lists:map(fun(A) -> list_to_atom(A) end, string:tokens(String, ",")), B1};
    true -> helper_common:extract_length_coded(false, B)
  end;
draw_value(B, Field_metadata, {_, _, _, Is_unsigned}) ->
  Int_length = case Field_metadata#field_metadata.type of
    ?MYSQL_TYPE_TINY -> 8;
    ?MYSQL_TYPE_SHORT -> 16;
    ?MYSQL_TYPE_LONG -> 32;
    ?MYSQL_TYPE_LONGLONG -> 64;
    ?MYSQL_TYPE_INT24 -> 32
  end,
  draw_integer_value(B, Is_unsigned, Int_length).

%% @spec draw_integer_value(B::binary(), Is_unsigned::boolean(), Int_length::integer()) -> {Value, Rest::binary()}
%% @doc Extracts integer value from binary.
%%
draw_integer_value(B, true, Int_length) ->
  <<Value:Int_length/little-integer, B1/binary>> = B,
  {Value, B1};
draw_integer_value(B, false, Int_length) ->
  <<Value:Int_length/signed-little-integer, B1/binary>> = B,
  {Value, B1}.

