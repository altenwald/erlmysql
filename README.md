# ErlMysql MySQL client for Erlang #

Copyright (c) 2010-2014 Alexei Krasnopolski 

__Authors:__ "Alexei Krasnopolski" ([`krasnop@bellsouth.net`](mailto:krasnop@bellsouth.net)).

### Introduction ###

The client allows to connect to MySQL server and execute SQL query. It provides connection pooling mechanizm for concurrency 
efficience. The client is written on Erlang and very closely follows MySQL Connector/C interface. It can be called MySQL Connector/Erlang
but has some restrictions in functionality (recent version has no SSL support). Design is based on protocol description from 
[http://mysql.timesoft.cc/doc/internals/en/index.html] (http://mysql.timesoft.cc/doc/internals/en/index.html). 
The client was tested on MySQL server version 5.1.51, 5.5.9 and 5.6.2 on Windows/Linux/MacOSX boxes.


### Architecture ###

MySQL client is an OTP application. Top level component is datasource supervisor (datasource_sup Erlang module) that is 
monitoring a child datasource processes (datasource is a connection pool in other words). Each datasource (or connection pool) 
is managing of a set of connection processes (generic services). Connection pool keeps reusable connection processes that can 
be retrived and returned by client software. Structure of the application is shown on figure below. 
The client is using external Erlang Resource Pool project [https://sourceforge.net/projects/erlpoo](https://sourceforge.net/projects/erlpool) 
for connection pool implementation with customized connection_factory.erl.

[MySQL](http://erlmysql.sourceforge.net/mySQL.png)

* mysql client - OTP application that combines application and supervisor behaviour. The supervisor is managing a numbers of datasource processes. It implements a functionality to create new or delete existed datasources.
* datasource module - contains connection pool process (gen_server) that is managing a few connection processes. The datasource module can manipulate underlying resource (connection) pool. The pool helps to reuse opened connection objects. Connection pool is automatically created when datasource is initialized.
* connection - generic server process that incapsulates all functionality concerned connection to DB, quering, prepared statement operations and so on.


### Getting started ###

To start with the client you have to complete at least first two steps below:

1. Install MySQL [http://dev.mysql.com/downloads](http://dev.mysql.com/downloads).
2. Install Erlang [http://www.erlang.org/download.html](http://www.erlang.org/download.html).
3. Install Eclipse IDE [http://www.eclipse.org/](http://www.eclipse.org/) with Erlide plugin [http://erlide.sourceforge.net/](http://erlide.sourceforge.net/). This is optional. But I am doing developing of the software using these wonderful tools and highly recommend ones.
4. Install OpenSSL framework (If your workstation does not have it yet) [http://www.openssl.org/](http://www.openssl.org/) because Erlang **crypto** is depended on the library. Commonly OpenSSL is already installed in Linux hosts.

Next step is running the examples. Suppose you install MySQL server as **'localhost'** that opened 3306 port for listening a TCP/IP 
connections from clients. You have to adjust root user account: set **'root'@'localhost'** with 'root' password and create account 
**'user'@'localhost'/'user'**. I will not teach you how using Erlang, just remind you:

* You have to use Rebar to build project. Issue command rebar clean get-deps compile.
* Start Erlang shell.
* Set up current directory with eshell command `c:cd(".../ebin").`
* Add path for dependency `code:add_path(".../deps/rsrc_pool/ebin").`


        1> c:cd("/home/alexei/eclipse-workspace/erl.mysql.client/ebin").
        /home/alexei/eclipse-workspace/erl.mysql.client/ebin
        ok
        2> code:add_path("/home/alexei/eclipse-workspace/erl.mysql.client/deps/rsrc_pool/ebin").
        true

Now we can issue commands for MySQL server. First at all we need to start application *'mysql_client'* that represents describing client.

        2> my:start_client().

Next step is a creating of datasource process with associated connection pool. First lets create description of our datasource. Load records definitions to console environment to make our next steps more clear: 

        3> rr(".././include/client_records.hrl").
        [client_options,connection,datasource,eof_packet,
        error_packet,field_metadata,metadata,mysql_decimal,
        mysql_error,mysql_time,ok_packet,ok_stmt_packet,packet,
        rs_header,server_info,server_status]

Assign record #datasource{} to DS_def value:

        4> DS_def = #datasource{
        4> host = "localhost", 
        4> port = 3306,
        4> database = "",
        4> user = "root",
        4> password = "root"}.
        #datasource{name = undefined,host = "localhost",port = 3306,
                    database = [],user = "root",password = "root",
                    flags = #client_options{charset_number = 33,
                                            long_password = 1,found_rows = 0,
                                            long_flag = 1,connect_with_db = 1,
                                            no_schema = 0,compress = 0,odbc = 0,
                                            local_files = 0,ignore_space = 0,
                                            protocol_41 = 1,interactive = 1,ssl = 0,
                                            ignore_sigpipe = 0,transactions = 1,
                                            reserved = 0,secure_connection = 1,
                                            multi_statements = 1,multi_results = 1,
                                            trans_isolation_level = default}}

And finally create new datasource object: 

        5> my:new_datasource(data_source, DS_def).
        {ok,<0.46.0>}


We have now datasource named 'data_source' that we can use for connecting to DB. Lets establish a connection to the server:

        6> Cntn = datasource:get_connection(data_source).
        <0.48.0>


Command above retrives connection from pool and if connection pool has no idle connection then connection factory creates new one. We can check obtained connection: 

        7> connection:connection_record(Cntn).
        #connection{
            socket = #Port<0.668>,
            ds_def = 
                #datasource{
                    name = data_source,host = "localhost",port = 3306,
                    database = [],user = "root",password = "root",
                    flags = 
                        #client_options{
                            charset_number = 33,long_password = 1,found_rows = 0,
                            long_flag = 1,connect_with_db = 1,no_schema = 0,
                            compress = 0,odbc = 0,local_files = 0,ignore_space = 0,
                            protocol_41 = 1,interactive = 1,ssl = 0,
                            ignore_sigpipe = 0,transactions = 1,reserved = 0,
                            secure_connection = 1,...}},
            server_info = 
                #server_info{
                    protocol_version = 10,server_version = "5.1.60",
                    thread_Id = 97,
                    server_capabilities = 
                        #client_options{
                            charset_number = 8,long_password = 1,found_rows = 1,
                            long_flag = 1,connect_with_db = 1,no_schema = 1,
                            compress = 1,odbc = 1,local_files = 1,ignore_space = 1,
                            protocol_41 = 1,interactive = 1,ssl = 0,
                            ignore_sigpipe = 1,transactions = 1,reserved = 0,
                            secure_connection = 1,multi_statements = 0,
                            multi_results = 0,...},
                    server_status = 
                        #server_status{
                            inTransaction = false,autocommit = true,
                            moreResultExists = false,queryNoGoodIndexUsed = false,
                            queryNoIndexUsed = false,cursorExists = false,
                            lastRowSent = false,dbDropped = false,
                            noBackSlashEscapes = false,metadataChanged = false,
                            queryWasSlow = false,psOutParams = false},
                    scramble_buff = <<"Kw8M[aVj2ZO[^G&&+F]$">>}}



Please, see client_records.hrl file to interpret content of returned *'connection'* record. You can use value of Cntn for further work. To create database named *'testDB'* issue a command:

        8> connection:execute_query(Cntn, "CREATE DATABASE IF NOT EXISTS testDB").
        {#metadata{
            field_count = 0,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = [],param_metadata = []},
        [#ok_packet{
            affected_rows = 1,insert_id = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            warning_count = 0,message = []}]}


Now your MySQL server has 'testDB' database. Next step is a table creating.

        9> connection:execute_query(Cntn, 
        9> "CREATE TABLE testDB.sample_table (id bigint(20) NOT NULL AUTO_INCREMENT, 
        9>  name varchar(45) DEFAULT NULL, age int(10) DEFAULT 21, 
        9>  longtext_col longtext, PRIMARY KEY (id)) 
        9>  ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8").
        {#metadata{
            field_count = 0,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = [],param_metadata = []},
        [#ok_packet{
            affected_rows = 0,insert_id = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            warning_count = 0,message = []}]}


You can insert now some data rows into table 'sample_table':

        10> connection:execute_query(Cntn,
        10> "INSERT INTO testDB.sample_table(name) VALUES ('Alex'), ('John')").
        {#metadata{
            field_count = 0,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = [],param_metadata = []},
        [#ok_packet{
            affected_rows = 2,insert_id = 1,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            warning_count = 0,
            message = "&Records: 2  Duplicates: 0  Warnings: 0"}]}


And finally you can select a rows from the table: 

        11> connection:execute_query(Cntn, "SELECT * FROM testDB.sample_table").
        {#metadata{
            field_count = 4,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = true,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = 
                [#field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "id",origname = "id",
                    charsetnr = 63,length = 20,type = 8,
                    flags = <<3,66>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "name",origname = "name",
                    charsetnr = 33,length = 135,type = 253,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "age",origname = "age",
                    charsetnr = 63,length = 10,type = 3,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "longtext_col",
                    origname = "longtext_col",charsetnr = 33,
                    length = 4294967295,type = 252,
                    flags = <<16,0>>,
                    scale = 0,default = []}],
            param_metadata = []},
        [[1,"Alex",21,null],[2,"John",21,null]]}


### How interpret response ###

Allmost all functions of **connection**code> module returns a tuple that we can describe as mysql_response type: 

        mysql_response = {metadata(), result()},

        Where

        metadata() = #metadata{
            field_count = integer(), 
            server_status = list(#server_status{}), 
            field_metadata = list(#field_metadata{})
        },

        result() = [] | [E#ok_packet{}] | [E#ok_stmt_packet{}] | list(rowdata()),

        rowdata() = list(field_value()),

        field_value() = integer() | float() | string() | binary() | #mysql_time{} | #mysql_decimal{}


First element of the tuple is metadata record (see client_records.hrl), that keeps an information about fields involved in 
the SQL query. Second element is a list of result rows of query or command. Some commands return empty list of result or list 
of the single element (ok_packet or ok_stmt_packet). Other commands return list of rows of rowdata() type each of these in 
own turn is a list of field values (field_value() type). Lets investigate the result of last query more carefully. 
After some formating it will look like: 

        {
        #metadata{
            field_count = 4,
            param_count = 0,
            server_status = #server_status{
            inTransaction = false,
            autocommit = true,
            moreResultExists = false,
            queryNoGoodIndexUsed = false,
            queryNoIndexUsed = true,
            cursorExists = false,
            lastRowSent = false,
            dbDropped = false,
            noBackSlashEscapes = false,
            metadataChanged = false,
            queryWasSlow = false,
            psOutParams = false
            },
            field_metadata = [
            #field_metadata{
                catalog = "def",
                schema = "testDB",
                table = "sample_table",
                origtable = "sample_table",
                name = "id",
                origname = "id",
                charsetnr = 63,
                length = 20,
                type = 8,
                flags = <<3,66>>,
                scale = 0,
                default = []
            },
            #field_metadata{
                catalog = "def",
                schema = "testDB",
                table = "sample_table",
                origtable = "sample_table",
                name = "name",
                origname = "name",
                charsetnr = 33,
                length = 135,
                type = 253,
                flags = <<0,0>>,
                scale = 0,
                default = []
            },
            #field_metadata{
                catalog = "def",
                schema = "testDB",
                table = "sample_table",
                origtable = "sample_table",
                name = "age",
                origname = "age",
                charsetnr = 63,
                length = 10,
                type = 3,
                flags = <<0,0>>,
                scale = 0,
                default = []
            },
            #field_metadata{
                catalog = "def",
                schema = "testDB",
                table = "sample_table",
                origtable = "sample_table",
                name = "longtext_col",
                origname = "longtext_col",
                charsetnr = 33,
                length = 4294967295,
                type = 252,
                flags = <<16,0>>,
                scale = 0,
                default = []
            }
            ],
            param_metadata = []
        },
        [
            [1,"Alex",21,null],
            [2,"John",21,null]
        ]
        }


We can see that the query returns two rows each of these contains four fields: values of 'id' = 1,2; values of 'name' field = "Alex","John"; values of 'age' = 21,21 and 'longtext_col' = null, null. Also the query returns a whole description of each fields in records named 'field_metadata'.
How extract the value we need? It is easy. Suppose we need value of second column 'name' from second row of the query result 

        12> {_, Rows} = connection:execute_query(Cntn, "SELECT * FROM testDB.sample_table").
        {#metadata{
            field_count = 4,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = true,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = 
                [#field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "id",origname = "id",
                    charsetnr = 63,length = 20,type = 8,
                    flags = <<3,66>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "name",origname = "name",
                    charsetnr = 33,length = 135,type = 253,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "age",origname = "age",
                    charsetnr = 63,length = 10,type = 3,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "longtext_col",
                    origname = "longtext_col",charsetnr = 33,
                    length = 4294967295,type = 252,
                    flags = <<16,0>>,
                    scale = 0,default = []}],
            param_metadata = []},
        [[1,"Alex",21,null],[2,"John",21,null]]}
        13> Rows.
        [[1,"Alex",21,null],[2,"John",21,null]]
        14> [_,{rs_row_data,Row}] = Rows.
        [[1,"Alex",21,null],[2,"John",21,null]]
        15> Row.
        [2,"John",21,null]
        16> [_,Name] = Row.
        [2,"John",21,null]
        17> Name.
        "John"

Note that MySQL server returns result set fields as a strings and the client is trying to convert them to proprietary Erlang types. Possible convertions are shown in Table 1 below. 

### Cursor (client side) ###

There is more convenient way to retrieve data from response. We can create cursor object from response data on client side and use this cursor to navigate through set of records and fields. 

        18> Response = connection:execute_query(Cntn, "SELECT * FROM testDB.sample_table").
        {#metadata{ 
        ... output is skipped ...},
        [[1,"Alex",21,null],[2,"John",21,null]]}
        19> Cursor = cursor:new(Response).
        <0.77.0>

To navigate to next row we have to call cursor:next function or set desired index of row. 

        20> cursor:next(Cursor). %% step forward
        true
        21> cursor:set(Cursor, 1). %% set on cursor beginning
        true


Now we can get value of a field: 

        22> Name1 = cursor:get(Cursor, "name").
        "Alex"
        23> Age1 = cursor:get(Cursor, "age").
        21

Move to next row and get a field by index in current row: 

        25> cursor:next(Cursor).
        true
        26> Name2 = cursor:get(Cursor, 2).
        "John"

Other operations under cursor are:

* **reset**: set cursor pointer to a beginnig of the cursor.
* **next**: move pointer to the next position.
* **set**: set pointer to given position.
* **skip**: skip a few position ahead.
* **back**: move pointer to the previous position.

Let us investigate a 'foreach' function. The function returns list of values for given field from all rows in cursor: 

        19> cursor:foreach(Cursor, "name").
        ["Alex","John"]

Cursor object owns to process created it. Other processes cannot access the cursor so cursor cannot be used concurrently.

### Prepared statements ###

MySQL client protocol allows to define prepared statements: SQL query with placeholder (?) for parameters and using this prepared statement for querying of the server farther. The simple example of a prepared statement is here: 

        19>H = connection:get_prepared_statement_handle(Cntn, "SELECT * FROM testDB.sample_table WHERE id = ?").
        1

Function get_prepared_statement_handle returns handler of the prepared statement we will use in next operations. Now let's try to execute this prepared statement. 

        20>{_,[R|_]} = connection:execute_statement(Cntn, H, [8], [1]).
        {#metadata{
            field_count = 4,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = 
                [#field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "id",origname = "id",
                    charsetnr = 63,length = 20,type = 8,
                    flags = <<3,66>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "name",origname = "name",
                    charsetnr = 33,length = 135,type = 253,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "age",origname = "age",
                    charsetnr = 63,length = 10,type = 3,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "longtext_col",
                    origname = "longtext_col",charsetnr = 33,
                    length = 4294967295,type = 252,
                    flags = <<16,0>>,
                    scale = 0,default = []}],
            param_metadata = []},
        [[1,"Alex",21,null]]}
        21>R.
        [1,"Alex",21,null]

We pass to connection:execute_statement/4 parameters:

* **Cntn -** connection tuple,
* **H -** statement handle
* **[8] -** list of parameter types (in our case 8 - LONGLONG MySQL type)
* **[1] -** list of parameter values (in our case 1 - id of desired table record)

Result is first record of the table with "Alex" in 'name' field. MySQL server formats a rows of a result set in 
different ways when responses on query or executes prepared statement. To make some order in types of data the Erlang 
client is doing some convertions of field values. Table below describes a correspondence between types of field defined by 
DDL statement and types returned by server in response for query and requered to set for prepared statement parameters.


### Prepared statement cursor and result fetching ###

After a statement is prepared we can execute it under two modes. First kind of execution is default and immediately 
returns a result set of the prepared statement. Second one does not return a result set but create a cursor on the server side.
To retrieve a data from this cursor we can use fetch_statement command like this: 

        22>H1 = connection:get_prepared_statement_handle(Cntn, "SELECT * FROM testDB.sample_table WHERE id < ?").
        2
        23>LONGLONG = 8.
        8
        24>CURSOR_TYPE_READ_ONLY = 1.
        1
        25>{M,_} = connection:execute_statement(Cntn, H1, [LONGLONG], [1], CURSOR_TYPE_READ_ONLY, true).
        {#metadata{
            field_count = 4,param_count = 0,server_status = undefined,
            field_metadata = 
                [#field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "id",origname = "id",
                    charsetnr = 63,length = 20,type = 8,
                    flags = <<1,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "name",origname = "name",
                    charsetnr = 33,length = 135,type = 253,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "age",origname = "age",
                    charsetnr = 63,length = 10,type = 3,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "longtext_col",
                    origname = "longtext_col",charsetnr = 33,
                    length = 4294967295,type = 252,
                    flags = <<16,0>>,
                    scale = 0,default = []}],
            param_metadata = []},
        []}
        26>{_,R1} = connection:fetch_statement(Cntn, H1, M, 2).
        {#metadata{
            field_count = 4,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = true,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = 
                [#field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "id",origname = "id",
                    charsetnr = 63,length = 20,type = 8,
                    flags = <<1,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "name",origname = "name",
                    charsetnr = 33,length = 135,type = 253,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "age",origname = "age",
                    charsetnr = 63,length = 10,type = 3,
                    flags = <<0,0>>,
                    scale = 0,default = []},
                #field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "longtext_col",
                    origname = "longtext_col",charsetnr = 33,
                    length = 4294967295,type = 252,
                    flags = <<16,0>>,
                    scale = 0,default = []}],
            param_metadata = []},
        [[1,"Alex",21,null],[2,"John",21,null]]}

First line is a command to prepare a statement with handler H. Second line is a command to execute the prepared statement 
in cursor mode. The command does not return any result but we need get a metadata record for the following command. 
The next line is a fetch command that return 2 first rows from the server side cursor. A fetch command returns only binary 
packets of result set but skips field metadata. So we have to pass metadata record as a parameter to fetch command due to 
properly parse rows data.


### Errors, Exceptions ###

The Erlang MySQL client can detect some errors and throws an exceptions. A record #mysql_error represents information about the error. There are a few kind of the errors:

* **tcp -**: This kind of exception is thrown by the client while network or socket error is occured.
* **connection -**: This concerns server connection errors.
* **sqlquery -**: Errors arise during execution of SQL query.
* **statement -**: Prepared statement can prevent an execution of query with wrong set of parameter.
* **transaction -**: Error during transaction. The exception arises after transaction rollback.

Functional programming style does not welcome a using exceptions, so MySQL client uses exception if it can not be avoided, 
Otherwise a functions return #mysql_error{} record if execution is failed.

### Transactions ###

The client has a transaction support. If you need some persistance operations/queries wrap as an one transaction then just 
define a function that implements this queries. The function accesses only one parameter - Connection object. This function 
has to return #mysql_error{} record if any of wrapped query is failed or any data if all transaction query are successful. 
You need pass this function to my:transaction/2 command. The transaction command has two parameter: first one is a Connection 
object, second one is a function mentioned above. See example:

        27>F = fun (C) -> connection:execute_query(C, 
        27>"UPDATE testDB.sample_table SET age = age + 1 WHERE id = 1") end.
        #Fun<erl_eval.6.82930912>
        28>Result = case connection:transaction(Cntn, F) of
        28>#mysql_error{} -> io:format("Transaction is failed and rollbacked ~n"), failed;
        28>R2 -> R2 end.
        {#metadata{
            field_count = 0,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = true,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = [],param_metadata = []},
        [#ok_packet{
            affected_rows = 1,insert_id = 0,
            server_status = 
                #server_status{
                    inTransaction = true,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            warning_count = 0,
            message = "(Rows matched: 1  Changed: 1  Warnings: 0"}]}

connection:transaction/2 function is successfuly completed if the transaction is committed and it returns #mysql_err{} record if the transaction is rollbacked.
Finally we need to return connection to pool and free the resource: 

        18> datasource:return(data_source, Cntn).
        ok

### Compression ###

Compression protocol is supported if instance of MySQL supports it. To activate this feature set compress field in 
client_options record and pass the record to datasource definition when create new datasource object. 
Then established connection allows to talk to server with compession. Tips: when we are using compression we win in 
packets size but lost in processor time. Example of compressed connection establishment: 

        DS_def_compr = #datasource{
                host = "localhost", 
                port = 3306, 
                database = "testdb", 
                user = "root", 
                password = "root", 
                flags = #client_options{compress=1}
            },
            my:new_datasource(datasource_compr, DS_def_compr),
            Connection = datasource:get_connection(datasource_compr)

### Blob transfer ###

MySQL allows keep in blob table fields a huge amount of data: up to 4294967296 (16#100000000) bytes. To send a long data to 
server the MySQL client/server protocol defines SEND_LONG_DATA command. The command is a part of prepared statement execution 
cycle and can be used only within one.

Suppose we have a some table with column of LONGBLOB type and we need to update the field. First we have to create prepared 
statement: 

        29>Handle = connection:get_prepared_statement_handle(Connection, 
        29>"UPDATE some_table SET longtext_col= ? WHERE persist_id = ?").
        3

After that we can send to server long block of data that has size of 1000000 bytes: 

        30>connection:send_statement_long_parameter(Connection, Handle, 0, <<16#AA:8000000>>).
        ok

Third parameter of the function is a position number of given prepared statement parameter. We can apply the 
send_statement_long_parameter/4 a few times and all chunks will be merged in one huge data block. 
Now as we complete sending of statement parameter value to server we can finally execute the statement: 

        31>LONG_BLOB = 251.
        251
        32>connection:execute_statement(Connection, Handle, [LONG_BLOB, LONG], [null, 1]).
        {#metadata{
            field_count = 0,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = [],param_metadata = []},
        [#ok_packet{
            affected_rows = 1,insert_id = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            warning_count = 1,
            message = "(Rows matched: 1  Changed: 1  Warnings: 0"}]}


During execution we do not need to send blob parameter value, because it already is in the server. Please, note that MySQL 
server has limitation to maximum client packet size (max_allowed_packet = 1048576 by default). So you can not send chunk of 
long data more then max_allowed_packet, but you can send this chunks a few times as much as needed and server will concatenate 
them.

Server response has no limitations and we can query table with blob any size. Server will split huge packet to standard 
ones and ErlMySQL client merges them as needed. 

        33>connection:execute_query(Connection, "SELECT longtext_col FROM some_table WHERE persist_id = 1").
        {#metadata{
            field_count = 1,param_count = 0,
            server_status = 
                #server_status{
                    inTransaction = false,autocommit = true,
                    moreResultExists = false,queryNoGoodIndexUsed = false,
                    queryNoIndexUsed = false,cursorExists = false,
                    lastRowSent = false,dbDropped = false,
                    noBackSlashEscapes = false,metadataChanged = false,
                    queryWasSlow = false,psOutParams = false},
            field_metadata = 
                [#field_metadata{
                    catalog = "def",schema = "testDB",table = "sample_table",
                    origtable = "sample_table",name = "longtext_col",
                    origname = "longtext_col",charsetnr = 33,
                    length = 4294967295,type = 252,
                    flags = <<16,0>>,
                    scale = 0,default = []}],
            param_metadata = []},
        [[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0|...]]]}
















