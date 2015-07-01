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

%% @since 2014-03-08
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://krasnopolski.org/]
%% @version {@version}
%% @doc Client side DB cursor represents record set of query. Cursor allows easy navigate througth record set and retrieve
%% field values by name or index. Cursor owns to process created it and can not access concurrently.
%%


-module(cursor).

-include("client_records.hrl").
-include("client_settings.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  init/2,
%% Create
  new/1,
  new/2,
  size/1,
%% Navigation
  next/1,
  skip/2,
  back/1,
  reset/1,
  set/2,
%% Data access
  get/2,       
  foreach/2
]).

%% @spec new(DB_result_set::tuple()) -> pid()
%%
%% @doc Creates new cursor from result record set obtained after db query.
%%
new(DB_result_set) when is_tuple(DB_result_set) ->
  spawn_link(?MODULE, init, [DB_result_set, self()]).

%% @spec new(Name::atom(), DB_result_set::tuple()) -> pid()
%%
%% @doc Creates new cursor with regestered name from result record set obtained after db query.
%%
new(Name, DB_result_set) ->
  Pid = spawn_link(?MODULE, init, [DB_result_set, self()]),
  register(Name, Pid),
  Pid.

%% @spec size(CursorId) -> integer()
%% CursorId = pid() | atom()
%%
%% @doc Get size (number of rows) of the cursor.
%%
size(CursorId) ->
  CursorId ! {self(), size},
  receive
    Response -> Response
  end.

%% @spec next(CursorId) -> boolean()
%% CursorId = pid() | atom()
%%
%% @doc Move cursor pointer to the next position. Returns false if pointer already on the end of cursor.
%%
next(CursorId) ->
  CursorId ! {self(), next},
  receive
    Response -> Response
  end.

%% @spec skip(CursorId, I::integer()) -> integer()
%% CursorId = pid() | atom()
%%
%% @doc Skip <code>I</code> positions and set cursor pointer to the position. Returns number actualy skipped positions.
%%
skip(CursorId, I) ->
  CursorId ! {self(), {skip, I}},
  receive
    Response -> Response
  end.

%% @spec back(CursorId) -> boolean()
%% CursorId = pid() | atom()
%%
%% @doc Move cursor pointer back to the previous position. Returns false if pointer already on the beginning of cursor.
%%
back(CursorId) ->
  CursorId ! {self(), back},
  receive
    Response -> Response
  end.

%% @spec set(CursorId, I::integer()) -> boolean()
%% CursorId = pid() | atom()
%%
%% @doc Set <code>I</code> as a new positions of the cursor pointer. Returns false if <code>I</code> is out of the range.
%%
set(CursorId, I) ->
  CursorId ! {self(), {set, I}},
  receive
    Response -> Response
  end.

%% @spec reset(CursorId) -> boolean()
%% CursorId = pid() | atom()
%%
%% @doc Reset cursor pointer to the beginning position.
%%
reset(CursorId) ->
  CursorId ! {self(), reset},
  receive
    Response -> Response
  end.

%% @spec get(CursorId, Name) -> Result
%% CursorId = pid() | atom()
%% Name = string() | integer()
%% Result = integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{}
%%
%% @doc Get value of field with <code>Name</code>. <code>Name</code> can be string name of field or index in row list.
%%
get(CursorId, Name) ->
  CursorId ! {self(), {get, Name}},
  receive
    Response -> Response
  end.

% @spec foreach(CursorId, Name) -> Result
%% CursorId = pid() | atom()
%% Name = string() | integer()
%% Result = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%%
%% @doc Returns list of values of <code>Name</code> field from all rows of record set. <code>Name</code> can be string name of field or index in row list.
%%
foreach(CursorId, Name) ->
  CursorId ! {self(), {foreach, Name}},
  receive
    Response -> Response
  end.


%% @spec init(DB_result_set, From::pid()) -> any()
%% DB_result_set = {Metadata::#metadata{}, ResultSet} | #mysql_error{}
%% ResultSet = list(Row)
%% Row = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%% @doc initiate cursor process.
%% @private
init(#mysql_error{} = DB_result_set, _From) ->
  {stop, DB_result_set};
init({#metadata{field_metadata = Field_metadata}, ResultSet}, From) ->
  FieldNames = [{Name, Type} || #field_metadata{name = Name, type = Type} <- Field_metadata],
  run(new_state({From, 0, FieldNames, ResultSet, []}, 1)).

%% @spec run({From, Row, FieldNames, ResultSet, CurrentRow}) -> any()
%% From = pid()
%% Row = integer()
%% FieldNames = list({Name, Type})
%% ResultSet = list(list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{}))
%% CurrentRow = list(integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{})
%% @doc process loop.
%% @private
run({From, Row, FieldNames, ResultSet, CurrentRow} = State) ->
  receive
% --------- size ----------
    {From, size} -> 
      From ! length(ResultSet), 
      run(State);
% --------- next ----------
    {From, next} when (Row >= length(ResultSet)) and (Row =< 0) -> 
      From ! false, 
      run(State);
    {From, next} -> 
      From ! true, 
      run(new_state(State, Row + 1));
% --------- back ----------
    {From, back} when Row =< 1 -> 
      From ! false, 
      run(State);
    {From, back} -> 
      From ! true, 
      run(new_state(State, Row - 1));
% --------- skip ----------
    {From, {skip, I}} when (Row + I) >= length(ResultSet) -> 
      From ! 0, 
      run(State);
    {From, {skip, I}} -> 
      NewRow = 
        case Row + I of
          NR when NR > length(ResultSet) -> length(ResultSet);
          NR -> NR
        end,
      From ! NewRow - Row, 
      run(new_state(State, NewRow));
% --------- reset ----------
    {From, reset} when (Row == 1) -> 
      From ! true, 
      run(State);
    {From, reset} -> 
      From ! true, 
      run(new_state(State, 1));
% --------- set ----------
    {From, {set, I}} when (I >= length(ResultSet)) orelse (I < 1) -> 
      From ! false, 
      run(State);
    {From, {set, I}} when I == Row -> 
      From ! true, 
      run(State);
    {From, {set, I}} -> 
      From ! true, 
      run(new_state(State, I));
% --------- get ----------
    {From, {get, Name}} when is_list(Name) -> 
      Val = proplists:get_value(Name, CurrentRow),
      From ! Val, 
      run(State);
    {From, {get, Name}} when is_integer(Name) -> 
      {_, Val} = lists:nth(Name, CurrentRow),
      From ! Val, 
      run(State);
% --------- foreach ----------
    {From, {foreach, Name}} when is_list(Name) -> 
      case index(Name, FieldNames, 1) of
        0 -> From ! [], run(State);
        Idx ->
          Vals = [lists:nth(Idx, L) || L <- ResultSet],
          From ! Vals, run(State)
      end;
    {From, {foreach, Name}} when is_integer(Name) ->
      Vals = [lists:nth(Name, L) || L <- ResultSet],
      From ! Vals, 
      run(State);
% --------- error ----------
    {From, _} -> 
      From ! unknown_operation,
      run(State);
% --------- access ----------
    {Pid, _} -> 
      Pid ! access_forbidden,
      run(State)
  end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @spec index(Name::string(), FieldNames::list({Name::string(), Type::integer()}), Idx::integer()) -> integer()
%% @doc Returns number that defines position of Name in FieldNames list.
%% @private
index(_, [], _) -> 0;
index(Name, [{NameI, _} | FieldNames], Idx) ->
  case string:equal(Name, NameI) of
    true  -> Idx;
    false -> index(Name, FieldNames, Idx + 1)
  end.

%% @spec current_row(State::tuple(), Row::integer()) -> Current_Row
%% Current_Row = list({Name::string(), Value::term()})
%% @doc Returns new current row as a list of tuple Name=>Value.
%% @private
current_row({_, _, FieldNames, ResultSet, _}, Row) ->
  [{Name, Val} || {{Name, _Type}, Val} <- lists:zip(FieldNames, lists:nth(Row, ResultSet))].

%% @spec new_state(State::tuple(), Row::integer()) -> New_State::tuple()
%% @doc Returns new state for the process.
%% @private
new_state({From, _, FieldNames, ResultSet, _} = State, Row) ->
  {From, Row, FieldNames, ResultSet, current_row(State, Row)}.
