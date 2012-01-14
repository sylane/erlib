%% ===========================================================================
%% @doc        Module navtree unit tests.
%% @since      May 15, 2010
%% @version    1.0
%% @copyright  (c) 2009, Sebastien Merle <s.merle@gmail.com>
%% @author     Sebastien Merle <s.merle@gmail.com>
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

-module(test_navtree).

-author('Sebastien Merle <s.merle@gmail.com>').


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

-include("navtree.hrl").


%% --------------------------------------------------------------------
%% Imports
%% --------------------------------------------------------------------

-import(navtree, [new/1, new/2,
                  is_root/1,
                  is_leaf/1,
                  get_count/1,
                  get_path/1,
                  go_path/2,
                  go_top/1,
                  go_up/1,
                  go_down/2,
                  get_node/1,
				  get_id/1,
                  update_node/2,
                  add_child/2, add_child/3,
				  remove_child/2]).


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

-export([coverage/0]).


%% ====================================================================
%% Coverage
%% ====================================================================

coverage() ->
    Module = navtree,
    Package = "",
    RelBeamPath = io_lib:format("ebin/~w.beam", [Module]),
    RelSourcePath = io_lib:format("~s/src/~w.erl", [Package, Module]),
    RelReportPath = io_lib:format("~s/test/coverage_~w.html", [Package, Module]),
    RelPkgIncludePath = io_lib:format("~s/include", [Package]),
    BeamPath = code:which(Module),
    RootPath = re:replace(BeamPath, RelBeamPath, "", [{return, list}]),
    SourcePath = RootPath ++ RelSourcePath,
    ReportPath = RootPath ++ RelReportPath,
    IncludePath = RootPath ++ RelPkgIncludePath,
    {ok, Module} = cover:compile(SourcePath, [{i, IncludePath}]),
    ?MODULE:test(),
    cover:analyse_to_file(Module, ReportPath, [html]),
    code:purge(Module),
    code:delete(Module),
    ok.


%% ====================================================================
%% Unit Tests
%% ====================================================================

basic_test() ->
    test_list(new(root),
              [mk_check_full(root, true, true, [], 0),
               mk_add_child(a1, 1),
			   mk_check_full(root, true, false, [], 1),
               mk_add_child(a2, 2),
			   mk_check_full(root, true, false, [], 2),
               mk_add_child(a3, 3),
			   mk_check_full(root, true, false, [], 3),
               mk_go_down(2),
			   mk_check_full(a2, false, true, [2], 0),
               mk_add_child(b1, 1),
			   mk_check_full(a2, false, false, [2], 1),
               mk_add_child(b2, 2),
			   mk_check_full(a2, false, false, [2], 2),
               mk_go_down(1),
			   mk_check_full(b1, false, true, [2, 1], 0),
               mk_add_child(c1, 1),
			   mk_check_full(b1, false, false, [2, 1], 1),
               mk_add_child(c2, 2),
			   mk_check_full(b1, false, false, [2, 1], 2),
               mk_add_child(c3, 3),
			   mk_check_full(b1, false, false, [2, 1], 3),
               mk_go_down(3),
			   mk_check(c3, [2, 1, 3], 0),
               mk_update(c3u),
			   mk_check(c3u, [2, 1, 3], 0),
               mk_go_up(),
			   mk_check(b1, [2, 1], 3),
               mk_update(b1u),
			   mk_check(b1u, [2, 1], 3),
               mk_go_down(2),
			   mk_check(c2, [2, 1, 2], 0),
               mk_go_up(),
			   mk_check(b1u, [2, 1], 3),
               mk_go_down(3),
			   mk_check(c3u, [2, 1, 3], 0),
               mk_go_top(),
			   mk_check(root, [], 3),
               mk_go_down(3),
			   mk_check(a3, [3], 0),
               mk_add_child(d1, 1),
			   mk_check(a3, [3], 1),
               mk_add_child(d2, 2),
			   mk_check(a3, [3], 2),
               mk_add_child(d3, 3),
			   mk_check(a3, [3], 3),
               mk_go_down(1),
			   mk_check(d1, [3, 1], 0),
               mk_add_child(e1, 1),
			   mk_check(d1, [3, 1], 1),
               mk_go_down(1),
			   mk_check(e1, [3, 1, 1], 0),
               mk_add_child(f1, 1),
			   mk_check(e1, [3, 1, 1], 1),
               mk_add_child(f2, 2),
			   mk_check(e1, [3, 1, 1], 2),
               mk_go_down(2),
			   mk_check(f2, [3, 1, 1, 2], 0),
               mk_add_child(g1, 1),
			   mk_check(f2, [3, 1, 1, 2], 1),
               mk_go_down(1),
			   mk_check(g1, [3, 1, 1, 2, 1], 0),
               mk_update(g1u),
			   mk_check(g1u, [3, 1, 1, 2, 1], 0),
               mk_go_up(),
			   mk_check(f2, [3, 1, 1, 2], 1),
               mk_go_up(),
			   mk_check(e1, [3, 1, 1], 2),
               mk_go_down(1),
			   mk_check(f1, [3, 1, 1, 1], 0),
               mk_update(f1u),
			   mk_check(f1u, [3, 1, 1, 1], 0),
               mk_go_up(),
			   mk_check(e1, [3, 1, 1], 2),
               mk_go_up(),
			   mk_check(d1, [3, 1], 1),
               mk_add_child(e2, 2),
			   mk_check(d1, [3, 1], 2),
               mk_go_up(),
			   mk_check(a3, [3], 3),
               mk_update(a3u),
			   mk_check(a3u, [3], 3),
               mk_go_top(),
			   mk_check(root, [], 3),
               mk_update(r),
			   mk_check(r, [], 3),
               mk_go_path([2, 1, 3]),
			   mk_check(c3u, [2, 1, 3], 0),
               mk_go_path([3, 1, 1, 2, 1]),
			   mk_check(g1u, [3, 1, 1, 2, 1], 0),
               mk_go_path([2, 1]),
			   mk_check(b1u, [2, 1], 3),
               mk_go_path([3, 1, 1, 1]),
			   mk_check(f1u, [3, 1, 1, 1], 0),
               mk_go_path([3]),
			   mk_check(a3u, [3], 3),
               mk_go_path([]),
			   mk_check(r, [], 3),
               mk_go_path([3, 1, 2]),
			   mk_check(e2, [3, 1, 2], 0),
               mk_add_child(h1, 1),
			   mk_check(e2, [3, 1, 2], 1),
               mk_add_child(h2, 2),
			   mk_check(e2, [3, 1, 2], 2),
               mk_go_path([1]),
			   mk_check(a1, [1], 0),
               mk_update(a1u),
			   mk_check(a1u, [1], 0),
               mk_go_path([3, 1, 2, 2]),
			   mk_check(h2, [3, 1, 2, 2], 0)
               ]).

atom_test() ->
    test_list(new(r, root),
              [mk_check(root, [], 0),
               mk_add_child("A1", a1),
			   mk_check(root, [], 1),
			   mk_add_child("A2", a2),
               mk_add_child("A3", a3),
			   mk_check(root, [], 3),
			   mk_go_down(a2),
               mk_check("A2", [a2], 0),
			   mk_add_child("B1", b1),
               mk_add_child("B2", b2),
			   mk_check("A2", [a2], 2),
			   mk_go_down(b2),
			   mk_check("B2", [a2, b2], 0),
			   mk_add_child("C1", c1),
			   mk_check("B2", [a2, b2], 1),
			   mk_go_path([a2]),
			   mk_check("A2", [a2], 2),
			   mk_remove_child(b1),
			   mk_check("A2", [a2], 1),
			   mk_go_path([a2, b2]),
			   mk_check("B2", [a2, b2], 1)
			   ]).


%% ====================================================================
%% Internal Functions
%% ====================================================================

mk_check_full(ExpChild, ExpIsRoot, ExpIsLeaf, ExpPath, ExpChildCount) ->
    fun(T) ->
            case ExpIsRoot of
                true ->
                    ?assertMatch(?NAVTREE_MATCH_ROOT(), T),
                    ?assertEqual(true, is_root(T));
                false ->
                    ?assertNot(?MATCHES(?NAVTREE_MATCH_ROOT(), T)),
                    ?assertEqual(false, is_root(T))
            end,
            case ExpIsLeaf of
                true ->
                    ?assertMatch(?NAVTREE_MATCH_LEAF(), T),
                    ?assertEqual(true, is_leaf(T));
                false ->
                    ?assertNot(?MATCHES(?NAVTREE_MATCH_LEAF(), T)),
                    ?assertEqual(false, is_leaf(T))
            end,
            ?assertMatch(?NAVTREE_MATCH_CURRENT(ExpChild), T),
            ?assertEqual(ExpChild, get_node(T)),
            ?assertEqual(ExpChildCount, get_count(T)),
            ?assertEqual(ExpPath, get_path(T)),
            T
    end.

mk_check(ExpChild, ExpPath, ExpChildCount) ->
    fun(T) ->
            ?assertMatch(?NAVTREE_MATCH_CURRENT(ExpChild), T),
            ?assertEqual(ExpChild, get_node(T)),
            ?assertEqual(ExpChildCount, get_count(T)),
            ?assertEqual(ExpPath, get_path(T)),
            T
    end.

mk_add_child(Child, Id) ->
    fun(T0) ->
            Result = add_child(Id, Child, T0),
            ?assertMatch({Id, _}, Result),
            {_, T} = Result,
            T
    end.

mk_remove_child(Id) ->
    fun(T0) ->
            remove_child(Id, T0)
    end.

mk_go_down(Index) ->
    fun(T) -> go_down(Index, T) end.

mk_go_up() ->
    fun(T) -> go_up(T) end.

mk_go_top() ->
    fun(T) -> go_top(T) end.

mk_go_path(Path) ->
    fun(T) -> go_path(Path, T) end.

mk_update(Value) ->
    fun(T) ->
            update_node(Value, T)
    end.

test_list(_Tree, []) -> ok;
test_list(Tree, [Fun |Ops]) -> test_list(Fun(Tree), Ops).
