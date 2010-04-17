%% ===========================================================================
%% @doc        Utility functions for file path manipulation.
%% @since      Nov 28, 2009
%% @version    1.0
%% @copyright  (c) 2009, Sebastien Merle <s.merle@gmail.com>
%% @authors    Sebastien Merle <s.merle@gmail.com>
%% @end
%%
%% Copyright (c) 2009, Sebastien Merle <s.merle@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%   * Redistributions of source code must retain the above copyright
%%     notice, this list of conditions and the following disclaimer.
%%   * Redistributions in binary form must reproduce the above copyright
%%     notice, this list of conditions and the following disclaimer in the
%%     documentation and/or other materials provided with the distribution.
%%   * Neither the name of "erlib" nor the names of its contributors may be
%%     used to endorse or promote products derived from this software without
%%     specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%% ===========================================================================

-module(pathlib).

-author('Sebastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([cleanup/1,
         absname/1, absname/2]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Cleanup the specified path.
%%   "X/Y/../Z" -> "X/Z"
%%   "X/./Y" -> "X/Y"
%%   "X//Y" -> "X/Y"
%%   "X/Y/" -> "X/Y"
-spec cleanup(string()) -> string().
%% --------------------------------------------------------------------
cleanup(Path) -> cleanup_path(filename:split(Path), []).

%% --------------------------------------------------------------------
%% Same as filename:absname/1 but it cleans up the path.
-spec absname(string()) -> string().
%% --------------------------------------------------------------------
absname(Path) -> cleanup(filename:absname(Path)).

%% --------------------------------------------------------------------
%% Same as filename:absname/2 but it cleans up the path.
-spec absname(string(), string()) -> string().
%% --------------------------------------------------------------------
absname(Path, Dir) -> cleanup(filename:absname(Path, Dir)).


%% ====================================================================
%% Local Functions
%% ====================================================================

cleanup_path([], []) -> [];
cleanup_path([], Acc) -> filename:join(lists:reverse(Acc));
cleanup_path([".." |Es], []) -> cleanup_path(Es, [".."]);
cleanup_path([".." |Es], [".." |_] = Acc) -> cleanup_path(Es, [".." |Acc]);
cleanup_path([".." |Es], [_ |Acc]) -> cleanup_path(Es, Acc);
cleanup_path(["." |Es], Acc) -> cleanup_path(Es, Acc);
cleanup_path([E |Es], Acc) -> cleanup_path(Es, [E |Acc]).


%% ====================================================================
%% Tests
%% ====================================================================

cleanup_test() ->
    ?assertEqual("", cleanup("")),
    ?assertEqual("/", cleanup("/")),
    ?assertEqual("c:/", cleanup("c:/")),
    ?assertEqual("/a/b/c", cleanup("/a/b/c/")),
    ?assertEqual("/a/b/c", cleanup("/a/b/c")),
    ?assertEqual("a/b/c", cleanup("a/b/c")),
    ?assertEqual("a/b/c", cleanup("a/b/c/")),
    ?assertEqual("a/c", cleanup("a/b/../c")),
    ?assertEqual("c", cleanup("a/../b/../c")),
    ?assertEqual("../c", cleanup("a/../../b/../c")),
    ?assertEqual("../c", cleanup("a/../b/../../c")),
    ?assertEqual("../c", cleanup("../a/../b/../c")),
    ?assertEqual("../../../c", cleanup("../../../c")),
    ?assertEqual("../../c", cleanup("../../a/../c")),
    ?assertEqual("..", cleanup("../c/..")),
    ?assertEqual("", cleanup(".")),
    ?assertEqual("", cleanup("./")),
    ?assertEqual("", cleanup("./.")),
    ?assertEqual("", cleanup("././")),
    ?assertEqual("a/b/c", cleanup("./a/./b/./c")),
    ?assertEqual("", cleanup("./c/..")),
    ?assertEqual("a/b/c", cleanup("a//b//c//")),
    ok.
