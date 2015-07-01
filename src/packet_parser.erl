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
%% @doc This module incapsulates functions these are responsible for processing of MySQL binary packets. 
%% Binary packets came from MySQL server are parsing to the client specific Erlang records. 

-module(packet_parser).

%%
%% Include files
%%
-include("client_records.hrl").

%%
%% Import modules
%%
-import(packet_parser_binary, []).
-import(packet_parser_string, []).
-import(helper_common, []).

%%
%% Exported Functions
%%
-export([
  parse_server_response_packet/4 
]).

%%
%% API Functions
%%

%% @spec parse_server_response_packet(RS_seq_state::int(), B::binary(), Metadata::#metadata{}, Stmt::atom()) -> Result
%% Stmt = prepare | fetch | none
%% Result = #ok_packet{} | #ok_stmt_packet{} | #error_packet{} | #rs_header{} | #field_metadata{} | #eof_packet{} | #mysql_error{} |
%%          list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%% @doc Parses binary B to one of possible type of packet record. During processing uses parameters
%% RS_seq_state (position of the processing packet in response set sequence) and Stmt (if response for statement is executed)
%% are for flow control.
%%
parse_server_response_packet(RS_seq_state, <<0:8, B1/binary>> = B, Metadata, Stmt) when size(B1) =:= 0 ->
  parse_result_packet(RS_seq_state, B, Metadata, Stmt);
parse_server_response_packet(RS_seq_state, <<0:8, _:6, Two_bits:2/integer, _/binary>> = B, Metadata, none) 
                                when ((Two_bits =:= 0) orelse (Two_bits =:= 1)) and (RS_seq_state > 1) ->
  <<_:8, B1/binary>> = B,
  packet_parser_binary:parse_binary_row(B1, Metadata); % Binary Row Data packet
parse_server_response_packet(_RS_seq_state, <<0:8, B1/binary>>, _Metadata, none) ->
  parse_OK_packet(B1);        % OK packet
parse_server_response_packet(_RS_seq_state, <<0:8, B1/binary>>, _Metadata, prepare) ->
  parse_OK_statement_packet(B1);  % OK statement prepare packet
parse_server_response_packet(_RS_seq_state, <<0:8, B1/binary>>, Metadata, fetch) ->
  packet_parser_binary:parse_binary_row(B1, Metadata); % Binary Row Data packet
parse_server_response_packet(_RS_seq_state, <<16#ff:8, B1/binary>>, _Metadata, _Stmt) -> % Error Packet
  parse_error_packet(B1);
parse_server_response_packet(_RS_seq_state, <<16#fe:8, B1/binary>>, _Metadata, _Stmt) when size(B1) < 9 -> % EOF Packet
  parse_EOF_packet(B1);
parse_server_response_packet(RS_seq_state, B, Metadata, Stmt) -> % Result Packet
  parse_result_packet(RS_seq_state, B, Metadata, Stmt).

%%
%% Local Functions
%%

%% @spec parse_OK_packet(B::binary()) -> #ok_packet{}
%%
%% @doc TODO
%%
parse_OK_packet(B) ->
  {Affected_rows, B1} =helper_common:extract_length_coded_integer(B),
  {Insert_id, B2} =helper_common:extract_length_coded_integer(B1),
  <<Server_status:2/binary, Warning_count:16/little-integer, Message/binary>> = B2,
  #ok_packet{
      affected_rows = Affected_rows, 
      insert_id = Insert_id, 
      server_status =helper_common:server_status_to_record(Server_status), 
      warning_count = Warning_count, message = binary_to_list(Message)
  }.

%% @spec parse_OK_statement_packet(B::binary()) -> #ok_stmt_packet{}
%%
%% @doc TODO
%%
parse_OK_statement_packet(B) ->
  <<Stmt_handle:32/little-integer, Columns_number:16/little-integer, Params_number:16/little-integer, 0:8, Warning_count:16/little-integer>> = B,
  #ok_stmt_packet{stmt_handle = Stmt_handle, columns_number = Columns_number, params_number = Params_number, warn_count = Warning_count}.

%% @spec parse_error_packet(B::binary()) -> #error_packet{}
%%
%% @doc TODO
%%
parse_error_packet(B) ->
  <<ErrNo:16/little-integer, _:8, Sql_state:5/binary, Message/binary>> = B,
  #error_packet{errno = ErrNo, sqlstate = binary_to_list(Sql_state), message = binary_to_list(Message)}.

%% @spec parse_EOF_packet(B::binary()) -> #eof_packet{}
%%
%% @doc TODO
%%
parse_EOF_packet(B) ->
  <<Warning_count:16/little-integer, Status:2/binary>> = B,
  #eof_packet{warning_count = Warning_count, server_status =helper_common:server_status_to_record(Status)}.

%% @spec parse_result_packet(RS_seq_state::int(), B::binary(), Metadata::#metadata{}, Stmt::atom()) -> Result
%% Stmt = prepare | fetch | none
%% Result = #rs_header{} | #field_metadata{} | #mysql_error{} |
%%          list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%% @doc TODO
%%
parse_result_packet(0, B, Metadata, Stmt) when (Stmt =:= none) or (Stmt =:= cursor) ->
  {FieldCount, B1} = helper_common:extract_length_coded_integer(B),
  {Extra, B2} = helper_common:extract_length_coded_integer(B1),
  if
    size(B2) =:= 0 -> #rs_header{field_count = FieldCount, extra = Extra};
    true -> parse_result_packet(1, B, Metadata, none)
  end;
parse_result_packet(1, B, _Metadata, _) ->
  {Catalog, B1} = helper_common:extract_length_coded(false, B),
  {Schema, B2} = helper_common:extract_length_coded(false, B1),
  {Table, B3} = helper_common:extract_length_coded(false, B2),
  {OrigTable, B4} = helper_common:extract_length_coded(false, B3),
  {Name, B5} = helper_common:extract_length_coded(false, B4),
  {OrigName, B6} = helper_common:extract_length_coded(false, B5),
  <<_:8, CharSet:16/little-integer, Length:32/little-integer, Type:8/integer, Flags:16/bitstring,
    Scale:8/integer, _:16, DefaultBin/binary>> = B6,
  {Default, Empty} = helper_common:extract_length_coded(false, DefaultBin),
  if
    size(Empty) =:= 0 ->
      #field_metadata{catalog = Catalog, schema = Schema, table = Table, origtable = OrigTable, 
              name = Name, origname = OrigName, charsetnr = CharSet, length = Length,
              type = Type, flags = Flags, scale = Scale, default = Default};
    true ->
      #mysql_error{type = connection, message = "Packet format is wrong",
                   source = "parse_result_packet(1, B, M, _)"}
  end;
parse_result_packet(2, B, Metadata, none) -> packet_parser_string:parse_string_row(B, Metadata);
parse_result_packet(2, B, Metadata, Stmt) -> parse_result_packet(1, B, Metadata, Stmt).
