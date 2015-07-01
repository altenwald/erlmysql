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

%
% MySQL client default settings.
%
-define(ACTIVE, true).
-define(BUFFER_SIZE, 16#4000).
-define(MAX_PACKET_SIZE, 16#FFFFFF).
-define(RECV_TIMEOUT, 60000).
-define(SEND_TIMEOUT, 60000).
-define(CONN_TIMEOUT, 60000).
-define(GEN_SERVER_TIMEOUT, 300000).
