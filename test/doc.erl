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

%% @hidden
%% @since 2010-12-30
%% @copyright 2010-2014 Alexei Krasnopolski
%% @author Alexei Krasnopolski <krasnop@bellsouth.net> [http://crasnopolski.com/]
%% @version {@version}
%% @doc This module is running doc generation script.

-module(doc).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([gen/1]).

%%
%% API Functions
%%

gen([Dir, Version]) ->
	io:format(">>>> gen() in directory ~p ~n", [Dir]),
	edoc:application('', Dir, 
				[{def, [{version, Version}]},
				 {overview, [Dir, "/src/overview.edoc"]},
				 {source_path, [[Dir, "/examples/"]]}, 
				 {private, false},
				 {title, "MySQL client for Erlang"}, 
%% looks like a bug in edoc with stylesheet?
%%				 {stylesheet, "my.css"}, 
%%				 {stylesheet_file, [Dir,"/priv/my.css"]}, 
				 {sort_functions, false}, 
				 {includes, [[Dir,"/include/"]]}
				]).
