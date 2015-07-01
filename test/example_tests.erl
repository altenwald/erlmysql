%%
%% Copyright (C) 2011-2013 by krasnop@bellsouth.net (Alexei Krasnopolski)
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

%% @hidden
%% @since 2010-12-02
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc This module is running erlang unit tests.

-module(example_tests).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

%%
%% API Functions
%%
example_test_() ->
  [ 
    {"1. example", timeout, 100, fun() -> example:run() end}
  ].

