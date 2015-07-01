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
%% MySQL data types:
%%

-define(MYSQL_TYPE_DECIMAL, 0). 
-define(DECIMAL, ?MYSQL_TYPE_DECIMAL). 
-define(MYSQL_TYPE_TINY, 1).
-define(TINY, ?MYSQL_TYPE_TINY).
-define(MYSQL_TYPE_SHORT, 2).
-define(SHORT, ?MYSQL_TYPE_SHORT).
-define(MYSQL_TYPE_LONG, 3).
-define(LONG, ?MYSQL_TYPE_LONG).
-define(MYSQL_TYPE_FLOAT, 4).
-define(FLOAT, ?MYSQL_TYPE_FLOAT).
-define(MYSQL_TYPE_DOUBLE, 5).
-define(DOUBLE, ?MYSQL_TYPE_DOUBLE).
-define(MYSQL_TYPE_NULL, 6).
-define(NULL, ?MYSQL_TYPE_NULL).
-define(MYSQL_TYPE_TIMESTAMP, 7).
-define(TIMESTAMP, ?MYSQL_TYPE_TIMESTAMP).
-define(MYSQL_TYPE_LONGLONG, 8).
-define(LONGLONG, ?MYSQL_TYPE_LONGLONG).
-define(MYSQL_TYPE_INT24, 9).
-define(INT24, ?MYSQL_TYPE_INT24).
-define(MYSQL_TYPE_DATE, 10).
-define(DATE, ?MYSQL_TYPE_DATE).
-define(MYSQL_TYPE_TIME, 11).
-define(TIME, ?MYSQL_TYPE_TIME).
-define(MYSQL_TYPE_DATETIME, 12). 
-define(DATETIME, ?MYSQL_TYPE_DATETIME). 
-define(MYSQL_TYPE_YEAR, 13).
-define(YEAR, ?MYSQL_TYPE_YEAR).
-define(MYSQL_TYPE_NEWDATE, 14).
-define(NEWDATE, ?MYSQL_TYPE_NEWDATE).
-define(MYSQL_TYPE_VARCHAR, 15).
-define(VARCHAR, ?MYSQL_TYPE_VARCHAR).
-define(MYSQL_TYPE_BIT, 16).
-define(BIT, ?MYSQL_TYPE_BIT).
-define(MYSQL_TYPE_NEWDECIMAL, 246).
-define(NEWDECIMAL, ?MYSQL_TYPE_NEWDECIMAL).
-define(MYSQL_TYPE_ENUM, 247).
-define(ENUM, ?MYSQL_TYPE_ENUM).
-define(MYSQL_TYPE_SET, 248).
-define(SET, ?MYSQL_TYPE_SET).
-define(MYSQL_TYPE_TINY_BLOB, 249).
-define(TINY_BLOB, ?MYSQL_TYPE_TINY_BLOB).
-define(MYSQL_TYPE_MEDIUM_BLOB, 250).
-define(MEDIUM_BLOB, ?MYSQL_TYPE_MEDIUM_BLOB).
-define(MYSQL_TYPE_LONG_BLOB, 251).
-define(LONG_BLOB, ?MYSQL_TYPE_LONG_BLOB).
-define(MYSQL_TYPE_BLOB, 252).
-define(BLOB, ?MYSQL_TYPE_BLOB).
-define(MYSQL_TYPE_VAR_STRING, 253).
-define(VAR_STRING, ?MYSQL_TYPE_VAR_STRING).
-define(MYSQL_TYPE_STRING, 254).
-define(STRING, ?MYSQL_TYPE_STRING).
-define(MYSQL_TYPE_GEOMETRY, 255).
-define(GEOMETRY, ?MYSQL_TYPE_GEOMETRY). %% total 26 types

-define(MYSQL_TYPE_UNSIGNED, 32768).
-define(UNSIGNED, ?MYSQL_TYPE_UNSIGNED).

%%
%% MySQL data flags:
%%

-define(NOT_NULL_FLAG, 1).              %% Field can't be NULL 
-define(PRI_KEY_FLAG, 2).               %% Field is part of a primary key 
-define(UNIQUE_KEY_FLAG, 4).            %% Field is part of a unique key 
-define(MULTIPLE_KEY_FLAG, 8).          %% Field is part of a key 
-define(BLOB_FLAG, 16).                 %% Field is a blob 
-define(UNSIGNED_FLAG, 32).             %% Field is unsigned 
-define(ZEROFILL_FLAG, 64).             %% Field is zerofill 
-define(BINARY_FLAG, 128).              %% Field is binary   
-define(ENUM_FLAG, 256).                %% field is an enum 
-define(AUTO_INCREMENT_FLAG, 512).      %% field is a autoincrement field 
-define(TIMESTAMP_FLAG, 1024).          %% Field is a timestamp 
-define(SET_FLAG, 2048).                %% field is a set 
-define(NO_DEFAULT_VALUE_FLAG, 4096).   %% Field doesn't have default value 
-define(ON_UPDATE_NOW_FLAG, 8192).      %% Field is set to NOW on UPDATE 
-define(NUM_FLAG, 32768).               %% Field is num (for clients) 

%%
%% MySQL prepared statement flags (cursor types)
%%

-define(CURSOR_TYPE_NO_CURSOR, 0).
-define(CURSOR_TYPE_READ_ONLY, 1).
-define(CURSOR_TYPE_FOR_UPDATE, 2).
-define(CURSOR_TYPE_SCROLLABLE, 4).

