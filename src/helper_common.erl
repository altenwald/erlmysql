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

%% @since 2011-04-25
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc Utility functions.

-module(helper_common).

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
-export([
  reverse_binary/1, 
  status_to_string/1, 
  server_status_to_record/1,
  type_to_atom/1,
  parse_data_flags/1,
  extract_length_coded_integer/1,
  extract_length_coded/2,
  binary_to_client_flags_record/2,
  client_flags_record_to_binary/1,
  binary_to_datetime/1,
  integer_to_length_coded_binary/1,
  binary_to_length_coded_binary/1,
  string_to_length_coded_binary/1,
  atom_list_to_string/1,
  decimal_to_lc_binary/1,
  datetime_to_lc_binary/1,
  date_to_lc_binary/1,
  time_to_lc_binary/1
]).

%%
%% API Functions
%%
-spec reverse_binary(Binary::binary()) -> binary().
-spec binary_to_client_flags_record(Lang::integer(), Binary::binary()) -> #client_options{}.
-spec client_flags_record_to_binary(ClientFlags::#client_options{}) -> binary().
-spec binary_to_datetime(Binary::binary()) -> #mysql_time{}.

%% @spec reverse_binary(Binary::binary()) -> binary()
%%
%% @doc Reverses an order of bytes in Binary.
%%
reverse_binary(<<>>) -> <<>>;
reverse_binary(<<B:1/binary, R/binary>>) -> <<(reverse_binary(R))/binary, B/binary>>.

%% @spec status_to_string(Server_status::#server_status{}) -> string()
%%
%% @doc Generates string representation of MySQL server status.
%%
status_to_string(#server_status{inTransaction = Tr, autocommit = AC, moreResultExists = MR,
                queryNoGoodIndexUsed = NGI, queryNoIndexUsed = NI, cursorExists = CE, lastRowSent = LR,
                dbDropped = DD, noBackSlashEscapes = NBS, metadataChanged = MG, queryWasSlow = QS, 
                psOutParams = OP}) ->
  lists:concat([
    "Server status: " 
    , "\n           In Transaction - ", Tr 
    , "\n               Autocommit - ", AC
    , "\n       More result exists - ", MR
    , "\n Query no good Index used - ", NGI
    , "\n      Query no Index used - ", NI
    , "\n            Cursor exists - ", CE
    , "\n            Last row sent - ", LR
    , "\n               DB dropped - ", DD
    , "\n     No Backslash escapes - ", NBS
    , "\n         Metadata changed - ", MG
    , "\n           Query was slow - ", QS
    , "\n     PS output parameters - ", OP
    , "\n"
  ]).  

%% @spec server_status_to_record(Binary::binary()) -> #server_status{} 
%%
%% @doc Parses binary to server status record.
%%
server_status_to_record(<<LastRowSent:1, CursorExists:1, NoIdx:1, NoGoodIdx:1, MoreResultExists:1, _:1, 
  Autocommit:1, InTransaction:1, _:3, PSOutParams:1, QueryWasSlow:1, MetadataChanged:1, NoBSEscapes:1, 
  DBDropped:1>>) ->
  #server_status{inTransaction = InTransaction =:= 1,
                autocommit = Autocommit =:= 1,
                moreResultExists = MoreResultExists =:= 1,
                queryNoGoodIndexUsed = NoGoodIdx =:= 1,
                queryNoIndexUsed = NoIdx =:= 1,
                cursorExists = CursorExists =:= 1,
                lastRowSent = LastRowSent =:= 1,
                dbDropped = DBDropped =:= 1,
                noBackSlashEscapes = NoBSEscapes =:= 1,
                metadataChanged = MetadataChanged =:= 1,
                queryWasSlow = QueryWasSlow =:= 1,
                psOutParams = PSOutParams =:= 1}.

%% @spec client_flags_record_to_binary(ClientFlags::#client_options{}) -> binary()
%%
%% @doc Generates binary from client flags record.
%
client_flags_record_to_binary(ClientFlags) ->
  <<
    (ClientFlags#client_options.local_files):1,
    (ClientFlags#client_options.odbc):1,
    (ClientFlags#client_options.compress):1,
    (ClientFlags#client_options.no_schema):1,
    (ClientFlags#client_options.connect_with_db):1,
    (ClientFlags#client_options.long_flag):1,
    (ClientFlags#client_options.found_rows):1,
    (ClientFlags#client_options.long_password):1,

    (ClientFlags#client_options.secure_connection):1,
    (ClientFlags#client_options.reserved):1,
    (ClientFlags#client_options.transactions):1,
    (ClientFlags#client_options.ignore_sigpipe):1,
    (ClientFlags#client_options.ssl):1,
    (ClientFlags#client_options.interactive):1,
    (ClientFlags#client_options.protocol_41):1,
    (ClientFlags#client_options.ignore_space):1,

    0:6,
    (ClientFlags#client_options.multi_results):1,
    (ClientFlags#client_options.multi_statements):1,
    0:8
  >>.

%% @spec binary_to_client_flags_record(Lang::integer(), Binary::binary()) -> #client_options{}
%%
%% @doc Extracts #client_options{} record from Binary.
%
binary_to_client_flags_record(Lang, <<Local_files:1, Odbc:1, Compress:1, No_schema:1, Connect_with_db:1,
    Long_flag:1, Found_rows:1, Long_password:1, Secure_connection:1, _R:1, Transactions:1,
    Ignore_sigpipe:1, Ssl:1, Interactive:1, Protocol_41:1, Ignore_space:1>>) ->
  #client_options{
    charset_number = Lang,
    long_password = Long_password,
    found_rows = Found_rows,
    long_flag = Long_flag,
    connect_with_db = Connect_with_db,
    no_schema = No_schema,
    compress = Compress,
    odbc = Odbc,
    local_files = Local_files,
    ignore_space = Ignore_space,
    protocol_41 = Protocol_41,
    interactive = Interactive,
    ssl = Ssl,
    ignore_sigpipe = Ignore_sigpipe,
    transactions = Transactions,
    secure_connection = Secure_connection,
    multi_statements = 0,
    multi_results = 0
  }.

%% @spec type_to_atom(MySQL_Type::integer()) -> atom() 
%%
%% @doc Converts integer constants represent MySQL types to atoms.
%%
type_to_atom(?MYSQL_TYPE_DECIMAL) -> decimal;
type_to_atom(?MYSQL_TYPE_TINY) -> tiny; 
type_to_atom(?MYSQL_TYPE_SHORT) -> short;
type_to_atom(?MYSQL_TYPE_LONG) -> long;
type_to_atom(?MYSQL_TYPE_FLOAT) -> float;
type_to_atom(?MYSQL_TYPE_DOUBLE) -> double;
type_to_atom(?MYSQL_TYPE_NULL) -> null;
type_to_atom(?MYSQL_TYPE_TIMESTAMP) -> timestamp;
type_to_atom(?MYSQL_TYPE_LONGLONG) -> long_long;
type_to_atom(?MYSQL_TYPE_INT24) -> int24;
type_to_atom(?MYSQL_TYPE_DATE) -> date;
type_to_atom(?MYSQL_TYPE_TIME) -> time;
type_to_atom(?MYSQL_TYPE_DATETIME) -> datetime; 
type_to_atom(?MYSQL_TYPE_YEAR) -> year;
type_to_atom(?MYSQL_TYPE_NEWDATE) -> new_date;
type_to_atom(?MYSQL_TYPE_VARCHAR) -> varchar;
type_to_atom(?MYSQL_TYPE_BIT) -> bit;
type_to_atom(?MYSQL_TYPE_NEWDECIMAL) -> new_decimal;
type_to_atom(?MYSQL_TYPE_ENUM) -> enum;
type_to_atom(?MYSQL_TYPE_SET) -> set;
type_to_atom(?MYSQL_TYPE_TINY_BLOB) -> tiny_blob;
type_to_atom(?MYSQL_TYPE_MEDIUM_BLOB) -> medium_blob;
type_to_atom(?MYSQL_TYPE_LONG_BLOB) -> long_blob;
type_to_atom(?MYSQL_TYPE_BLOB) -> blob;
type_to_atom(?MYSQL_TYPE_VAR_STRING) -> var_string;
type_to_atom(?MYSQL_TYPE_STRING) -> string;
type_to_atom(?MYSQL_TYPE_GEOMETRY) -> geometry.

%% @spec parse_data_flags(Binary::binary()) -> [atom()] 
%%
%% @doc Generates list of atoms these represent MySQL data flags are set.
%%
parse_data_flags(<<Binary:1, Zerofill:1, Unsigned:1, Blob:1, 
                   Multiple_Key:1, Unique_Key:1, Primary_Key:1, Not_Null:1,
                   Num:1, _:1, On_Update_Now:1, No_Default_Value:1, 
                   Set:1, Timestamp:1, Auto_Increment:1, Enum:1>>) ->
  DefList = [{Not_Null, not_null}, {Primary_Key, primary_key}, {Unique_Key, unique_key}, {Multiple_Key, multiple_key},
             {Blob, blob}, {Unsigned, unsigned}, {Zerofill, zerofill}, {Binary, binary}, {Enum, enum}, 
             {Auto_Increment, auto_increment}, {Timestamp, timestamp}, {Set, set}, {No_Default_Value, no_default_value},
             {On_Update_Now, on_update_now}, {Num, num}],
  [X || {1, X} <- DefList].  

%% @spec extract_length_coded_integer(Binary::binary()) -> {Value::integer(), Rest_of_binary::binary()} 
%%
%% @doc Extracts lenght coded integer from binary and returns a tuple that contains a value of extracted integer and rest of binary.
%%
extract_length_coded_integer(<<>>) -> {0, <<>>};
extract_length_coded_integer(<<255:8, B/binary>>) -> {0, B};
extract_length_coded_integer(<<254:8, Value:64/little-integer, B/binary>>) -> {Value, B};
extract_length_coded_integer(<<253:8, Value:24/little-integer, B/binary>>) -> {Value, B};
extract_length_coded_integer(<<252:8, Value:16/little-integer, B/binary>>) -> {Value, B};
extract_length_coded_integer(<<251:8, B/binary>>) -> {null, B};
extract_length_coded_integer(<<N:8, B/binary>>) -> {N, B}.

%% @spec extract_length_coded(Is_binary::boolean(), Binary::binary()) -> {Value::term(), Rest_of_binary::binary()}
%%
%% @doc Extracts lenght coded binary/string from binary stream and returns a tuple that contains
%% an extracted binary/string and rest of original binary.
%%
extract_length_coded(Is_binary, B) ->
  case extract_length_coded_integer(B) of
    {null, B1} -> {null, B1};
    {L, B1} -> <<Bytes:L/binary, B2/binary>> = B1,
      {if Is_binary -> Bytes; true -> binary_to_list(Bytes) end, B2}
  end.

%% @spec binary_to_datetime(Binary::binary()) -> #mysql_time{}
%%
%% @doc Converts SQL Datetime value represented as binary to record mysql_time.
%%
binary_to_datetime(<<FieldSize:8/integer, V0:FieldSize/binary, B1/binary>>) ->
  case V0 of
    <<>> -> {#mysql_time{}, B1};
    <<Year:16/little-integer, Month:8/integer, Day:8/integer, V1/binary>> ->
      case V1 of
        <<>> ->
          {#mysql_time{year = Year, month = Month, day = Day}, B1};
        <<Hour:8/integer, Minute:8/integer, Second:8/integer, SecPart/binary>> ->
          Second_Part =
          case SecPart of
            <<>> -> 0;
            <<Sec_part:32/little-integer>> -> Sec_part
          end,
          {#mysql_time{year = Year, month = Month, day = Day, 
                       hour = Hour, minute = Minute, second = Second, second_part = Second_Part}, B1}
      end
  end.

%% @spec integer_to_length_coded_binary(Value::integer()) -> binary() | #mysql_error{}
%%
%% @doc Converts integer value to MySQL binary format
%%
integer_to_length_coded_binary(Int) when Int =< 250 -> 
  <<Int:8/integer>>;
integer_to_length_coded_binary(Int) when Int =< 16#FFFF -> 
  <<252:8, Int:16/little-integer>>;
integer_to_length_coded_binary(Int) when Int =< 16#FFFFFF -> 
  <<253:8, Int:24/little-integer>>;
integer_to_length_coded_binary(Int) when Int =< 16#FFFFFFFFFFFFFFFF -> 
  <<254:8, Int:64/little-integer>>;
integer_to_length_coded_binary(_) ->
  #mysql_error{type = statement, source="helper_common:integer_to_length_coded_binary/1", message = "Parameter is too long."}.

%% @spec binary_to_length_coded_binary(Value::binary()) -> binary()
%%
%% @doc Converts Erlang binary value to MySQL binary format
%%
binary_to_length_coded_binary(Binary) ->
  Size = byte_size(Binary),
  <<(integer_to_length_coded_binary(Size))/binary, Binary/binary>>.

%% @spec string_to_length_coded_binary(Value::string()) -> binary()
%%
%% @doc Converts Erlang string to MySQL binary format
%%
string_to_length_coded_binary(String) ->
  binary_to_length_coded_binary(list_to_binary(String)).

%% @spec decimal_to_lc_binary(Value::#mysql_decimal{}) -> binary()
%%
%% @doc Converts Erlang mysql_decimal record to MySQL binary format
%%
decimal_to_lc_binary(#mysql_decimal{int = IntegerPart, fraction = FractionalPart}) ->
  string_to_length_coded_binary([IntegerPart, ".", FractionalPart]).

%% @spec datetime_to_lc_binary(Value::#mysql_time{}) -> binary()
%%
%% @doc Converts Erlang mysql_time record to MySQL datetime binary format
%%
datetime_to_lc_binary(#mysql_time{year = Year, month = Month, day = Day, 
            hour = Hour, minute = Minute, second = Second, second_part = Second_Part}) ->
  SP = if
    Second_Part =/= 0 -> <<Second_Part:32/little-integer>>;
    true -> <<>>
  end,  
  HMS = if
    Hour =/= 0 orelse Minute =/= 0 orelse Second =/= 0 -> 
      <<Hour:8/integer, Minute:8/integer, Second:8/integer, SP/binary>>;
    true -> <<>>
  end,  
  DT = if
    Year =/= 0 orelse Month =/= 0 orelse Day =/= 0 -> 
      <<Year:16/little-integer, Month:8/integer, Day:8/integer, HMS/binary>>;
    true -> <<>>
  end,
  <<(byte_size(DT)):8/integer, DT/binary>>.

%% @spec date_to_lc_binary(Value::#mysql_time{}) -> binary()
%%
%% @doc Converts Erlang mysql_time record to MySQL date binary format
%%
date_to_lc_binary(Date) ->
  datetime_to_lc_binary(Date#mysql_time{hour=0, minute=0, second=0, second_part=0}).

%% @spec time_to_lc_binary(Value::#mysql_time{}) -> binary()
%%
%% @doc Converts Erlang mysql_time record to MySQL time binary format
%%
time_to_lc_binary(#mysql_time{neg = Neg, day = Day, hour = Hour, minute = Minute, second = Second, second_part = Second_Part}) ->
  SP = if
    Second_Part =/= 0 -> <<Second_Part:32/little-integer>>;
    true -> <<>>
  end,  
  Negative = if Neg -> 1; true -> 0 end,
  DHMS = if
    Day =/= 0 orelse Hour =/= 0 orelse Minute =/= 0 orelse Second =/= 0 -> 
      <<Negative:8/integer, Day:32/integer-little, Hour:8/integer, Minute:8/integer, Second:8/integer, SP/binary>>;
    true -> <<>>
  end,  
  <<(byte_size(DHMS)):8/integer, DHMS/binary>>.

%% @spec atom_list_to_string(Atoms::list()) -> string()
%%
%% @doc Converts list of atoms to a string that contains string representation of the atoms separated by comma.
%%
atom_list_to_string(Atoms) ->
  lists:foldl(
    fun(Atom, [])  -> atom_to_list(Atom); 
       (Atom, Acc) -> lists:concat([Acc, ",", Atom]) 
    end, 
    [], 
    Atoms
  ).
