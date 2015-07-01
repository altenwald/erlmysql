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

%% @type server_status() = #server_status{}. The server_status() record contains information about status of SQL server after query.<br/>
%% -record(<strong>server_status</strong>, {
%% <dl>
%%   <dt>inTransaction = false::boolean()</dt><dd>- Transaction has started.</dd>
%%   <dt>autocommit = false::boolean()</dt><dd>- Server in auto_commit mode.</dd>
%%   <dt>moreResultExists = false::boolean()</dt><dd>- Multi query - next query exists.</dd>
%%   <dt> queryNoGoodIndexUsed = false::boolean()</dt><dd>- .</dd>
%%   <dt>queryNoIndexUsed = false::boolean()</dt><dd>- .</dd>
%%   <dt>cursorExists = false::boolean()</dt><dd>- read-only non-scrollable cursor for a query was opened.</dd>
%%   <dt>lastRowSent = false::boolean()</dt><dd>- read-only cursor is exhausted.</dd>
%%   <dt>noBackSlashEscapes = true::boolean()</dt><dd>- .</dd>
%%   <dt>queryWasSlow = false::boolean()</dt><dd>- this query was logged to the slow query log.</dd>
%%   <dt>psOutParams = false::boolean()</dt><dd>- To mark ResultSet containing output parameter values.</dd>
%% </dl>
%% }).
-record(server_status, 
  {
    inTransaction = false::boolean(),           % Transaction has started 
    autocommit = false::boolean(),              % Server in auto_commit mode
    moreResultExists = false::boolean(),        % Multi query - next query exists
    queryNoGoodIndexUsed = false::boolean(), 
    queryNoIndexUsed = false::boolean(), 
    cursorExists = false::boolean(),            % read-only non-scrollable cursor for a query was opened
    lastRowSent = false::boolean(),             % read-only cursor is exhausted
    dbDropped = false::boolean(),               % A database was dropped
    noBackSlashEscapes = true::boolean(), 
    metadataChanged = false::boolean(),         %
    queryWasSlow = false::boolean(),            % this query was logged to the slow query log
    psOutParams = false::boolean()              % To mark ResultSet containing output parameter values
  }
).


%% @type client_options() = #client_options{}. The record keeps client options that server is capable to support for this client and this connection.<br/>
%% -record(<strong>client_options</strong>, {
%% <dl>
%%   <dt>charset_number = 33::0..255</dt><dd>- Character set number: default utf-8 (code= 33).</dd>
%%   <dt>long_password = 1::0|1</dt><dd>- new more secure passwords.</dd>
%%   <dt>found_rows  = 0::0|1</dt><dd>- Found instead of affected rows.</dd>
%%   <dt>long_flag = 1::0|1</dt><dd>- Get all column flags.</dd>
%%   <dt>connect_with_db = 1::0|1</dt><dd>- One can specify db on connect.</dd>
%%   <dt>no_schema = 0::0|1</dt><dd>- Don't allow database.table.column.</dd>
%%   <dt>compress = 0::0|1</dt><dd>- Can use compression protocol.</dd>
%%   <dt>odbc = 0::0|1</dt><dd>- Odbc client.</dd>
%%   <dt>local_files = 0::0|1</dt><dd>- Can use LOAD DATA LOCAL.</dd>
%%   <dt>ignore_space = 0::0|1</dt><dd>- Ignore spaces before '('.</dd>
%%   <dt>protocol_41 = 1::0|1</dt><dd>- New 4.1 protocol.</dd>
%%   <dt>interactive = 1::0|1</dt><dd>- This is an interactive client.</dd>
%%   <dt>ssl = 0::0|1</dt><dd>- Switch to SSL after handshake.</dd>
%%   <dt>ignore_sigpipe = 0::0|1</dt><dd>- IGNORE sigpipes.</dd>
%%   <dt>transactions  = 1::0|1</dt><dd>- Client knows about transactions.</dd>
%%   <dt>reserved = 0::0|1</dt><dd>- Old flag for 4.1 protocol.</dd>
%%   <dt>secure_connection = 1::0|1</dt><dd>- New 4.1 authentication.</dd>
%%   <dt>multi_statements = 1::0|1</dt><dd>- Enable/disable multi-stmt support.</dd>
%%   <dt>multi_results = 1::0|1</dt><dd>- Enable/disable multi-results.</dd>
%%   <dt>trans_isolation_level = default::read_uncommitted | read_committed | repeatable_read | serializable</dt>
%%     <dd>- transaction isolation level.
%%       <ul>
%%         <li><strong>read_uncommitted</strong> - given session can read uncommitted updates of other session.</li>
%%         <li><strong>read_committed</strong> - given session can read only committed updates of other session.</li>
%%         <li><strong>repeatable_read</strong> - given session can read only snapshot of committed updates of other session.</li>
%%         <li><strong>serializable</strong> - the same as repeatable read but given session blocks own reads.</li>
%%       </ul>
%%     </dd>
%% </dl>
%% }).
-record(client_options, 
  {
    charset_number = 33::0..255,      % Character set number: default utf-8 (code= 33)
    long_password = 1::0|1,           % new more secure passwords
    found_rows  = 0::0|1,             % Found instead of affected rows
    long_flag = 1::0|1,               % Get all column flags
    connect_with_db = 1::0|1,         % One can specify db on connect
    no_schema = 0::0|1,               % Don't allow database.table.column
    compress = 0::0|1,                % Can use compression protocol
    odbc = 0::0|1,                    % Odbc client
    local_files = 0::0|1,             % Can use LOAD DATA LOCAL

    ignore_space = 0::0|1,            % Ignore spaces before '('
    protocol_41 = 1::0|1,             % New 4.1 protocol
    interactive = 1::0|1,             % This is an interactive client
    ssl = 0::0|1,                     % Switch to SSL after handshake
    ignore_sigpipe = 0::0|1,          % IGNORE sigpipes
    transactions  = 1::0|1,           % Client knows about transactions
    reserved = 0::0|1,                % Old flag for 4.1 protocol
    secure_connection = 1::0|1,       % New 4.1 authentication 

    multi_statements = 1::0|1,        % Enable/disable multi-stmt support
    multi_results = 1::0|1,           % Enable/disable multi-results
    trans_isolation_level =           % transaction isolation level
      default                         % default isolation level
      ::
      read_uncommitted |              % given session can read uncommitted updates of other session
      read_committed |                % given session can read only committed updates of other session
      repeatable_read |               % given session can read only snapshot of committed updates of other session
      serializable                    % the same as repeatable read but given session blocks own reads 
  }
).

%% @type server_info() = #server_info{}. The server_info() contains information that SQL server sent during handshake about itself. 
%% -record(<strong>server_info</strong>, {
%% <dl>
%%   <dt>protocol_version::integer()</dt><dd>- protocol version.</dd>
%%   <dt>server_version::string()</dt><dd>- server version.</dd>
%%   <dt>thread_Id::integer()</dt><dd>- current connection thread ID.</dd>
%%   <dt>server_capabilities::#client_options{}</dt><dd>- server capabilities.</dd>
%%   <dt>server_status::#server_status{}</dt><dd>- server status.</dd>
%%   <dt>scramble_buff::binary()</dt><dd>- scrumble buffer for password encryption.</dd>
%% </dl>
%% }).
-record(server_info,
  {
    protocol_version::integer(), 
    server_version::string(), 
    thread_Id::integer(), 
    server_capabilities::#client_options{}, 
    server_status::#server_status{}, 
    scramble_buff::binary()
  }
).

%% @type datasource() = #datasource{}. datasource record information describes connections pool. 
%% The information is using for creating connections of the pool.<br/>
%% -record(<strong>datasource</strong>, {
%% <dl>
%%   <dt>name :: atom()</dt><dd>- datasource name.</dd>
%%   <dt>host = "localhost" :: string()</dt><dd>- server host name or IP.</dd>
%%   <dt>port = 3306 :: integer()</dt><dd>- host port number.</dd>
%%   <dt>database = "" :: string()</dt><dd>- database name.</dd>
%%   <dt>user :: string()</dt><dd>- user logon name.</dd>
%%   <dt>password :: string()</dt><dd>- password.</dd>
%%   <dt>flags = #client_options{} :: #client_options{}</dt><dd>- client options for connections of the datasource.</dd>
%% </dl>
%% }).
-record(datasource,
  {
    name :: atom(),
    host = "localhost" :: string(),
    port = 3306 :: integer(),
    database = "" :: string(),
    user :: string(),
    password :: string(),
    flags = #client_options{} :: #client_options{}
  }
).

%% @type connection() = #connection{}. The record keeps information that helps connection server runs.<br/>
%% -record(<strong>connection</strong>, {
%% <dl>
%%   <dt>socket :: port()</dt><dd>- tcp socket linked to MySQL server.</dd>
%%   <dt>owner :: pid()</dt><dd>- pid of process that borrows this connection from connection pool. undefined if the connection is idle.</dd>
%%   <dt>ds_def :: #datasource{}</dt><dd>- datasource record.</dd>
%%   <dt>server_info :: #server_info{}</dt><dd>- server info record.</dd>
%% </dl>
%% }).
-record(connection,
  {
    socket :: port(),
    owner :: pid(),
    ds_def :: #datasource{},
    server_info :: #server_info{} 
  }
).

%% @type packet() = #packet{}. represents binary packet for low level network connection.<br/>
%% -record(<strong>packet</strong>, {
%% <dl>
%%   <dt>continue = false::boolean()</dt><dd>- indicates it is not last packet in sequence.</dd>
%%   <dt>seq_number = 0::integer()</dt><dd>- number of the packet in sequence.</dd>
%%   <dt>uncompressed_size = 0::integer()</dt><dd>- size of packet before compression.</dd>
%%   <dt>body = `<<>>'::binary()</dt><dd>- binary body of the packet.</dd>
%% </dl>
%% }).
-record(packet, 
  {
    continue = false::boolean(), 
    seq_number = 0::integer(), 
    uncompressed_size = 0::integer(), 
    body = <<>>::binary()
  }
).

%% @type ok_packet() = #ok_packet{}. represents OK packet received from server after generic query or command.<br/>
%% -record(<strong>ok_packet</strong>, {
%% <dl>
%%   <dt>affected_rows::integer()</dt><dd>- number affected rows in table(s) during processing query.</dd>
%%   <dt>insert_id::integer()</dt><dd>- insert query returns generated id for new row.</dd>
%%   <dt>server_status::#server_status{}</dt><dd>- server status after query.</dd>
%%   <dt>warning_count::integer()</dt><dd>- number of warnings.</dd>
%%   <dt>message::string()</dt><dd>- warn/error message.</dd>
%% </dl>
%% }).
-record(ok_packet, 
  {
    affected_rows::integer(), 
    insert_id::integer(), 
    server_status::#server_status{}, 
    warning_count::integer(), 
    message::string()
  }
).

%% @type ok_stmt_packet() = #ok_stmt_packet{}. represents OK packet received from server after statement prepare command.<br/>
%% -record(<strong>ok_stmt_packet</strong>, {
%% <dl>
%%   <dt>stmt_handle::integer()</dt><dd>- prepared statement handle for using with following commands.</dd>
%%   <dt>columns_number::integer()</dt><dd>- number of columns in prepared statement.</dd>
%%   <dt>params_number::integer()</dt><dd>- number of parameters in prepared statement.</dd>
%%   <dt>warn_count::integer()</dt><dd>- number of warnings.</dd>
%% </dl>
%% }).
-record(ok_stmt_packet, 
  {
    stmt_handle::integer(), 
    columns_number::integer(), 
    params_number::integer(), 
    warn_count::integer()
  }
).

%% @type error_packet() = #error_packet{}. represents ERROR packet received from server after generic query or command.<br/>
%% -record(<strong>error_packet</strong>, {
%% <dl>
%%   <dt>errno::integer()</dt><dd>- MySQL error number.</dd>
%%   <dt>sqlstate::string()</dt><dd>- MySQL state identifire.</dd>
%%   <dt>message::string()</dt><dd>- error message.</dd>
%% </dl>
%% }).
-record(error_packet, 
  {
    errno::integer(), 
    sqlstate::string(), 
    message::string()
  }
).

%% @type eof_packet() = #eof_packet{}. represents end of file (EOF) packet received from server after generic query or command.<br/>
%% -record(<strong>eof_packet</strong>, {
%% <dl>
%%   <dt>warning_count::integer()</dt><dd>- number of warnings.</dd>
%%   <dt>server_status::#server_status{}</dt><dd>- server status after query.</dd>
%% </dl>
%% }).
-record(eof_packet, 
  {
    warning_count::integer(), 
    server_status::#server_status{}
  }
).

%% @type rs_header() = #rs_header{}. represents a header of record set.<br/>
%% -record(<strong>rs_header</strong>, {
%% <dl>
%%   <dt>field_count::integer()</dt><dd>- number of fields in record set.</dd>
%%   <dt>extra::any()</dt><dd>- extra information.</dd>
%% </dl>
%% }).
-record(rs_header, 
  {
    field_count::integer(), 
    extra::any() %% TODO is it any, not integer ?
  }
).

%% @type field_metadata() = #field_metadata{}. represents field metadata of record set.<br/>
%% -record(<strong>field_metadata</strong>, {
%% <dl>
%%   <dt>catalog::string()</dt><dd>- catalog name (unused in recent versions).</dd>
%%   <dt>schema::string()</dt><dd>- schema (database) name.</dd>
%%   <dt>table::string()</dt><dd>- table name from sql query (after AS).</dd>
%%   <dt>origtable::string()</dt><dd>- original table name from schema.</dd>
%%   <dt>name::string()</dt><dd>- field name from sql query (after AS).</dd>
%%   <dt>origname::string()</dt><dd>- original field name from schema.</dd>
%%   <dt>charsetnr::integer()</dt><dd>- code of character set.</dd>
%%   <dt>length::integer()</dt><dd>- length of the field.</dd>
%%   <dt>type::0..255</dt><dd>- type of the field (see mysql_types.hrl).</dd>
%%   <dt>flags::bitstring()</dt><dd>- flags (see mysql_types.hrl).</dd>
%%   <dt>scale::0..255</dt><dd>- scale.</dd>
%%   <dt>default::binary()</dt><dd>- default value.</dd>
%% </dl>
%% }).
-record(field_metadata, 
  {
    catalog::string(), 
    schema::string(), 
    table::string(), 
    origtable::string(), 
    name::string(), 
    origname::string(), 
    charsetnr::integer(), 
    length::integer(), 
    type::0..255, 
    flags::bitstring(), 
    scale::0..255, 
    default::binary()
  }
).

%% @type metadata() = #metadata{}. represents metadata of record set.<br/>
%% -record(<strong>metadata</strong>, {
%% <dl>
%%   <dt>field_count=0::integer()</dt><dd>- field count in the query.</dd>
%%   <dt>param_count=0::integer()</dt><dd>- parameter count in prepared statement.</dd>
%%   <dt>server_status::#server_status{}</dt><dd>- server status after query.</dd>
%%   <dt>field_metadata = []::list(#field_metadata{})</dt><dd>- field metadata is a list of metadata 
%% for each field in the query.</dd>
%%   <dt>param_metadata = []::list(#field_metadata{})</dt><dd>- parameter metadata is a list of metadata 
%% for each parameter in the prepared statement..</dd>
%% </dl>
%% }).
-record(metadata, 
  {
    field_count=0::integer(), 
    param_count=0::integer(),
    server_status::#server_status{}, 
    field_metadata = []::list(#field_metadata{}),
    param_metadata = []::list(#field_metadata{})
  }
).

%% @type mysql_time() = #mysql_time{}. represents value of time/date field.<br/>
%% -record(<strong>mysql_time</strong>, {
%% <dl>
%%   <dt>neg = false::boolean()</dt><dd>- sign of the field.</dd>
%%   <dt>year = 0::integer()</dt><dd>- year.</dd>
%%   <dt>month = 0::0..12</dt><dd>- month.</dd>
%%   <dt>day = 0::0..31</dt><dd>- day of month.</dd>
%%   <dt>hour = 0::0..23</dt><dd>- hour.</dd>
%%   <dt>minute = 0::0..59</dt><dd>- minute.</dd>
%%   <dt>second = 0::0..59</dt><dd>- second.</dd>
%%   <dt>second_part = 0</dt><dd>- second part.</dd>
%% </dl>
%% }).
-record(mysql_time, 
  {
    neg = false::boolean(), 
    year = 0::integer(), 
    month = 0::0..12, 
    day = 0::0..31, 
    hour = 0::0..23, 
    minute = 0::0..59, 
    second = 0::0..59, 
    second_part = 0
  }
).

%% @type mysql_decimal() = #mysql_decimal{}. represents value of decimal field.<br/>
%% -record(<strong>mysql_decimal</strong>, {
%% <dl>
%%   <dt>int = ""::string()</dt><dd>- integer part.</dd>
%%   <dt>fraction = ""::string()</dt><dd>- fraction part.</dd>
%% </dl>
%% }).
-record(mysql_decimal, 
  {
    int = []::string(), 
    fraction = []::string()
  }
).

%% @type mysql_error() = #mysql_error{}. Record represents an exception that is thrown by a client's module.<br/> 
%% -record(<strong>mysql_error</strong>, {
%% <dl>
%%   <dt>type:: tcp | connection | sqlquery | statement | transaction</dt><dd>- .</dd>
%%   <dt>errno = none:: none | integer()</dt><dd>- .</dd>
%%   <dt>sqlstate = []::string()</dt><dd>- .</dd>
%%   <dt>source = []::string()</dt><dd>- .</dd>
%%   <dt>message = []::string()</dt><dd>- .</dd>
%% </dl>
%% }).
-record(mysql_error, 
  {
    type:: tcp | connection | sqlquery | statement | transaction | pooling, 
    errno = none:: none | integer(),
    sqlstate = []::string(),
    source = []::string(), 
    message = []::string()
  }
).
