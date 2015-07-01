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

%%
%% MySQL commands:
%%

-define(CLOSE, 1:8).
-define(SELECT_DB, 2:8).
-define(EXEC_QUERY, 3:8).
-define(GET_FIELD_LIST, 4:8).
-define(PING, 14:8). 
-define(CHANGE_USER, 17:8). 
-define(PREPARE_STATEMENT, 22:8).
-define(EXECUTE_STATEMENT, 23:8).
-define(SEND_STATEMENT_LONG_PARAMETER, 24:8).
-define(CLOSE_STATEMENT, 25:8).
-define(RESET_STATEMENT, 26:8).
-define(FETCH_STATEMENT, 28:8).

