%% ===========================================================================
%% @doc        Module optp unit tests.
%% @since      Apr 24, 2010
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

-module(test_optp).

-author('Sebastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Imports
%% --------------------------------------------------------------------

-import(optp, [new/2, new/3]).
-import(optp, [chain/1, chain/2]).
-import(optp, [parse/2]).

-import(optp, [iparam/4, iparam/5]).
-import(optp, [ichoice/5, ichoice/6]).
-import(optp, [iaccum/4, iaccum/5]).
-import(optp, [iflag/3, iflag/4]).
-import(optp, [icount/3, icount/4]).
-import(optp, [command/1, command/2]).

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

-export([debug/0, debug/1]).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

-record(opt, {str = "default",
               atom = default,
               int = 18,
               num = 1.2,
               choice = default,
               flag = false,
               count = 0,
               accum = []}).


%% ===================================================================
%% Debuging Functions
%% ====================================================================

test_parse1(Str) ->
    P = new(#opt{},
            [iparam("s", "string", string, #opt.str),
             iparam("a", "atom", atom, #opt.atom),
             iparam("i", "integer", integer, #opt.int),
             iparam("n", "number", number, #opt.num),
             ichoice("c", "choice", atom, #opt.choice,
                     [one, two, three]),
             iflag("f", "flag", #opt.flag),
             icount("o", "counter", #opt.count),
             iaccum("u", "accum", number, #opt.accum)]),
    parse(P, opts:split(Str)).

debug() -> debug("").

debug(Str) ->
    io:fwrite("Arguments: ~s~n", [Str]),
    io:fwrite("Defaults:  ~p~n", [#opt{}]),
    io:fwrite("Result:    ~p~n", [test_parse1(Str)]).


%% ===================================================================
%% Unit Tests
%% ====================================================================

-define(_equal1(V, E), ?_assertEqual([E, none, []], test_parse1(V))).
-define(_equal1b(V, E), ?_assertEqual(E, test_parse1(V))).
-define(_error1(V, E), ?_assertThrow({optp_error, E, _}, test_parse1(V))).

basic_test_() ->
    [?_equal1("", #opt{})].

short_str_test_() ->
    [?_error1("-s", need_argument),
     ?_error1("-s123", invalid_option),
     ?_equal1("-s XXX", #opt{str="XXX"}),
     ?_equal1("-s 123", #opt{str="123"}),
     ?_equal1("-s 123.5", #opt{str="123.5"}),
     ?_equal1("-s aaa -s bbb", #opt{str="bbb"})
    ].

long_str_test_() ->
    [?_error1("--string", need_argument),
     ?_error1("--string123", invalid_option),
     ?_error1("--string 123", need_argument),
     ?_equal1("--string=", #opt{str=""}),
     ?_equal1("--string=XXX", #opt{str="XXX"}),
     ?_equal1("--string=123", #opt{str="123"}),
     ?_equal1("--string=123.5", #opt{str="123.5"}),
     ?_equal1("--string=aaa --string=bbb", #opt{str="bbb"})
    ].

short_atom_test_() ->
    [?_error1("-a", need_argument),
     ?_error1("-a123", invalid_option),
     ?_equal1("-a true", #opt{atom=true}),
     ?_equal1("-a XXX", #opt{atom='XXX'}),
     ?_equal1("-a 123", #opt{atom='123'}),
     ?_equal1("-a 123.5", #opt{atom='123.5'}),
     ?_equal1("-a aaa -a bbb", #opt{atom=bbb})
    ].

long_atom_test_() ->
    [?_error1("--atom", need_argument),
     ?_error1("--atom aaa", need_argument),
     ?_error1("--atom123", invalid_option),
     ?_equal1("--atom=", #opt{atom=''}),
     ?_equal1("--atom=true", #opt{atom=true}),
     ?_equal1("--atom=XXX", #opt{atom='XXX'}),
     ?_equal1("--atom=123", #opt{atom='123'}),
     ?_equal1("--atom=123.5", #opt{atom='123.5'}),
     ?_equal1("--atom=aaa --atom=bbb", #opt{atom=bbb})
    ].

short_int_test_() ->
    [?_error1("-i", need_argument),
     ?_error1("-i123", invalid_option),
     ?_error1("-i XXX", invalid_argument),
     ?_error1("-i 123X", invalid_argument),
     ?_error1("-i 123.5", invalid_argument),
     ?_equal1("-i 123", #opt{int=123}),
     ?_equal1("-i -456", #opt{int=-456}),
     ?_equal1("-i 123 -i 456", #opt{int=456})
    ].

long_int_test_() ->
    [?_error1("--integer", need_argument),
     ?_error1("--integer=", invalid_argument),
     ?_error1("--integer123", invalid_option),
     ?_error1("--integer 123", need_argument),
     ?_error1("--integer=XXX", invalid_argument),
     ?_error1("--integer=123X", invalid_argument),
     ?_error1("--integer=123.5", invalid_argument),
     ?_equal1("--integer=123", #opt{int=123}),
     ?_equal1("--integer=-456", #opt{int=-456}),
     ?_equal1("--integer=123 --integer=456", #opt{int=456})
    ].

short_number_test_() ->
    [?_error1("-n", need_argument),
     ?_error1("-n123", invalid_option),
     ?_error1("-n XXX", invalid_argument),
     ?_error1("-n 123X", invalid_argument),
     ?_equal1("-n 123", #opt{num=123}),
     ?_equal1("-n -456", #opt{num=-456}),
     ?_equal1("-n 123.5", #opt{num=123.5}),
     ?_equal1("-n 1.842e+1", #opt{num=18.42}),
     ?_equal1("-n 1842.0e-2", #opt{num=18.42}),
     ?_equal1("-n -123.5", #opt{num=-123.5}),
     ?_equal1("-n -1.842e+1", #opt{num=-18.42}),
     ?_equal1("-n -1842.0e-2", #opt{num=-18.42}),
     ?_equal1("-n 123.4 -n 456.7", #opt{num=456.7})
    ].

long_number_test_() ->
    [?_error1("--number", need_argument),
     ?_error1("--number=", invalid_argument),
     ?_error1("--number 123", need_argument),
     ?_error1("--number123", invalid_option),
     ?_error1("--number=XXX", invalid_argument),
     ?_error1("--number=123X", invalid_argument),
     ?_equal1("--number=123", #opt{num=123}),
     ?_equal1("--number=-456", #opt{num=-456}),
     ?_equal1("--number=123.5", #opt{num=123.5}),
     ?_equal1("--number=1.842e+1", #opt{num=18.42}),
     ?_equal1("--number=1842.0e-2", #opt{num=18.42}),
     ?_equal1("--number=-123.5", #opt{num=-123.5}),
     ?_equal1("--number=-1.842e+1", #opt{num=-18.42}),
     ?_equal1("--number=-1842.0e-2", #opt{num=-18.42}),
     ?_equal1("--number=123.4 --number=456.7", #opt{num=456.7})
    ].

short_choice_test_() ->
    [?_error1("-c", need_argument),
     ?_error1("-c XXX", invalid_argument),
     ?_equal1("-c one", #opt{choice=one}),
     ?_equal1("-c two -c three", #opt{choice=three})
    ].

long_choice_test_() ->
    [?_error1("--choice", need_argument),
     ?_error1("--choice=", invalid_argument),
     ?_error1("--choice=XXX", invalid_argument),
     ?_equal1("--choice=one", #opt{choice=one}),
     ?_equal1("--choice=two --choice=three", #opt{choice=three})
    ].

short_flag_test_() ->
    [?_equal1("-f", #opt{flag=true}),
     ?_equal1("-fff", #opt{flag=true}),
     ?_equal1("-f -f", #opt{flag=true})
    ].

large_flag_test_() ->
    [?_error1("--flag=", argument_not_allowed),
     ?_error1("--flag=true", argument_not_allowed),
     ?_equal1("--flag", #opt{flag=true}),
     ?_equal1("--flag --flag", #opt{flag=true})
    ].

short_count_test_() ->
    [?_equal1("-o", #opt{count=1}),
     ?_equal1("-ooo", #opt{count=3}),
     ?_equal1("-o -o", #opt{count=2})
    ].

large_count_test_() ->
    [?_error1("--counter=", argument_not_allowed),
     ?_error1("--counter=true", argument_not_allowed),
     ?_equal1("--counter", #opt{count=1}),
     ?_equal1("--counter --counter --counter", #opt{count=3})
    ].

short_accum_test_() ->
    [?_error1("-u", need_argument),
     ?_error1("-u foo", invalid_argument),
     ?_equal1("-u 1", #opt{accum=[1]}),
     ?_equal1("-u 1 -u 2 -u 4", #opt{accum=[1, 2, 4]})
    ].

long_accum_test_() ->
    [?_error1("--accum", need_argument),
     ?_error1("--accum foo", need_argument),
     ?_error1("--accum=foo", invalid_argument),
     ?_equal1("--accum=1", #opt{accum=[1]}),
     ?_equal1("--accum=1 --accum=2", #opt{accum=[1, 2]})
    ].

short_mix_test_() ->
    [?_equal1("-oi 7 -s foo -c one -n 3.14 -foa spam",
              #opt{str="foo", atom=spam, int=7, num=3.14,
                    choice=one, flag=true, count=2}),
     ?_equal1("-fi 7 -c two -i 0 -f",
              #opt{choice=two, int=0, flag=true}),
     ?_equal1("-fu 7 -a foo -u 2 -f",
              #opt{atom=foo, flag=true, accum=[7, 2]})
    ].

long_mix_test_() ->
    [?_equal1("--counter --integer=7 --string=foo --choice=one "
                  "--number=3.14 --flag --counter --atom=spam",
                  #opt{str="foo", atom=spam, int=7, num=3.14,
                        choice=one, flag=true, count=2}),
     ?_equal1("--flag --integer=7 --choice=two --integer=0 --flag",
              #opt{choice=two, int=0, flag=true})
    ].

extra_args_test_() ->
    [?_equal1b("pim pam poum", [#opt{}, none, ["pim", "pam", "poum"]]),
     ?_equal1b("-fi 7 pim pam poum",
               [#opt{int=7, flag=true}, none, ["pim", "pam", "poum"]])
    ].

doublehyphen_test_() ->
    [?_equal1b("-i 18 --", [#opt{int=18}, none, []]),
     ?_equal1b("-a foo -- -f -i 18",
               [#opt{atom=foo}, none, ["-f", "-i", "18"]])
    ].

test_parse2(Str) ->
    P = new(#opt{},
            [iparam("a", "atom", #opt.atom, atom),
             iflag("f", "flag", #opt.flag),
             command("spam"),
             command("bacon"),
             command("beans")]),
    parse(P, opts:split(Str)).

-define(_equal2b(V, E), ?_assertEqual(E, test_parse2(V))).
-define(_error2(V, E), ?_assertThrow({optp_error, E, _}, test_parse2(V))).

simple_cmd_test() ->
    [?_error2("", need_command),
     ?_error2("-fa aaa", need_command),
     ?_error2("-f pim", invalid_command),
     ?_equal2b("spam", [#opt{}, "spam", nil, []]),
     ?_equal2b("-a aaa spam -f bacon beans",
               [#opt{atom=aaa}, "spam", nil, ["-f", "bacon", "beans"]])
    ].

test_parse3(Str) ->
    P11 = new(#opt{}, [iflag("f", "flag", #opt.flag)]),
    P1 = new(#opt{},
             [iparam("i", "integer", integer, #opt.int),
              icount("o", "counter", #opt.count),
              command("spam", [{parser, P11}])]),
    P2 = new(#opt{},
             [iparam("a", "atom", atom, #opt.atom),
              iflag("f", "flag", #opt.flag)]),
    P = new(#opt{},
            [iparam("a", "atom", atom, #opt.atom),
             ichoice("c", "choice", atom, #opt.choice,
                     [one, two, three]),
             command("bacon", [{parser, P1}]),
             command("beans", [{parser, P2}]),
             command("eggs")
            ]),
    parse(P, opts:split(Str)).

-define(_equal3b(V, E), ?_assertEqual(E, test_parse3(V))).
-define(_error3(V, E), ?_assertThrow({optp_error, E, _}, test_parse3(V))).

cmd_tree_test_() ->
    [?_error3("", need_command),
     ?_error3("-c one", need_command),
     ?_error3("-i 18 eggs", invalid_option),
     ?_error3("-c one foo", invalid_command),
     ?_equal3b("eggs", [#opt{}, "eggs", nil, []]),
     ?_equal3b("eggs foo bar", [#opt{}, "eggs", nil, ["foo", "bar"]]),
     ?_equal3b("-c one eggs", [#opt{choice=one}, "eggs", nil, []]),
     ?_equal3b("-c two eggs pim", [#opt{choice=two}, "eggs", nil, ["pim"]]),
     ?_equal3b("-a bbb beans", [#opt{atom=bbb}, "beans", #opt{}, none, []]),
     ?_equal3b("-a bbb beans foobar",
               [#opt{atom=bbb}, "beans", #opt{}, none, ["foobar"]]),
     ?_error3("beans -i 42", invalid_option),
     ?_equal3b("-a ccc beans -fa ddd",
               [#opt{atom=ccc}, "beans", #opt{flag=true, atom=ddd}, none, []]),
     ?_equal3b("-a ccc beans -fa ddd pim pam poum",
               [#opt{atom=ccc}, "beans", #opt{flag=true, atom=ddd},
                none, ["pim", "pam", "poum"]]),
     ?_error3("bacon -f foo", invalid_option),
     ?_error3("bacon -i 42", need_command),
     ?_error3("bacon -o foo", invalid_command),
     ?_equal3b("bacon -oi 18 spam foo bar",
               [#opt{}, "bacon", #opt{count=1, int=18},
                "spam", #opt{}, none, ["foo", "bar"]]),
     ?_error3("bacon spam -o", invalid_option),
     ?_equal3b("bacon -ooo spam -f",
               [#opt{}, "bacon", #opt{count=3},
                "spam", #opt{flag=true}, none, []])
    ].

test_parse4(Str) ->
    P1 = new(#opt{},
             [iparam("i", "integer", integer, #opt.int),
              icount("o", "counter", #opt.count),
              command("spam"), command("bacon")]),
    P2 = new(#opt{},
             [iparam("a", "atom", atom, #opt.atom),
              iflag("f", "flag", #opt.flag)]),
    P3 = new(#opt{},
            [ichoice("c", "choice", atom, #opt.choice,
                     [one, two, three]),
             command("eggs"), command("spam")]),
    P = chain([P1, P2, P3]),
    parse(P, opts:split(Str)).

-define(_equal4b(V, E), ?_assertEqual(E, test_parse4(V))).
-define(_error4(V, E), ?_assertThrow({optp_error, E, _}, test_parse4(V))).

parser_chaining_test_() ->
    [?_error4("", need_command),
     ?_error4("spam", need_command),
     ?_equal4b("spam spam spam",
               [#opt{}, "spam", nil,
                #opt{}, none,
                #opt{}, "spam", nil, ["spam"]]),
     ?_equal4b("bacon eggs foo bar",
               [#opt{}, "bacon", nil,
                #opt{}, none,
                #opt{}, "eggs", nil, ["foo", "bar"]]),
     ?_error4("-a foo spam eggs", invalid_option),
     ?_error4("spam -i 18 eggs", invalid_option),
     ?_error4("--atom foo spam eggs", invalid_option),
     ?_error4("spam --integer 18 eggs", invalid_option),
     ?_equal4b("-oo spam -fa foo -c three eggs foo bar",
               [#opt{count=2}, "spam", nil,
                #opt{flag=true, atom=foo}, none,
                #opt{choice=three}, "eggs", nil, ["foo", "bar"]]),
     ?_equal4b("spam -fc one eggs",
               [#opt{}, "spam", nil,
                #opt{flag=true}, none,
                #opt{choice=one}, "eggs", nil, []]),
     ?_equal4b("--counter spam --atom=foo --choice=three eggs foo bar",
               [#opt{count=1}, "spam", nil,
                #opt{atom=foo}, none,
                #opt{choice=three}, "eggs", nil, ["foo", "bar"]]),
     ?_equal4b("spam --flag --choice=one eggs",
               [#opt{}, "spam", nil,
                #opt{flag=true}, none,
                #opt{choice=one}, "eggs", nil, []])
    ].

-record(opt_deep, {sub1 = #opt{},
                    sub2 = #opt{}}).

test_parse5(Str) ->
    P = new(#opt_deep{},
            [iparam("i", "integer", integer, [#opt_deep.sub1, #opt.int]),
             iaccum("u", "accum", number, [#opt_deep.sub1, #opt.accum]),
             icount("o", "counter", [#opt_deep.sub2, #opt.count]),
             iparam("a", "atom", atom, [#opt_deep.sub2, #opt.atom])]),
    parse(P, opts:split(Str)).

-define(_equal5(V, E), ?_assertEqual([E, none, []], test_parse5(V))).
-define(_error5(V, E), ?_assertThrow({optp_error, E, _}, test_parse5(V))).

deep_index_test_() ->
    [?_equal5("", #opt_deep{}),
     ?_equal5("-o -i 18 -oo",
              #opt_deep{sub1 = #opt{int = 18},
                         sub2 = #opt{count = 3}}),
     ?_equal5("-u 1 -a foo -o -u 2 -i 18 -oo -u 3",
              #opt_deep{sub1 = #opt{int = 18, accum = [1, 2, 3]},
                         sub2 = #opt{count = 3, atom = foo}}),
     ?_equal5("--counter --integer=18 --counter",
              #opt_deep{sub1 = #opt{int = 18},
                         sub2 = #opt{count = 2}}),
     ?_equal5("--accum=1 --atom=foo -o --accum=2 -i 18 --counter --accum=3",
              #opt_deep{sub1 = #opt{int = 18, accum = [1, 2, 3]},
                         sub2 = #opt{count = 2, atom = foo}})
     ].

-define(SMPL_TPL_A, {{int, 0}, {flag, false}, {list, []}}).
-define(SMPL_TPL_B, {{str, undefined}, {count, 0}}).
-define(SMPL_TPL_C, {{atom, undefined}}).
-record(smpl_tpl, {a = ?SMPL_TPL_A,
                   b = ?SMPL_TPL_B}).

test_parse6(Str) ->
    Sub = new(?SMPL_TPL_C,
              [iparam("a", "atom", atom, [1, 2])]),
    P = new(#smpl_tpl{},
            [iparam("i", "integer", integer, [#smpl_tpl.a, 1, 2]),
             iflag("f", "flag", [#smpl_tpl.a, 2, 2]),
             iaccum("u", "accum", number, [#smpl_tpl.a, 3, 2]),
             iparam("s", "string", string, [#smpl_tpl.b, 1, 2]),
             icount("o", "counter", [#smpl_tpl.b, 2, 2]),
             command("cmd", Sub)]),

    parse(P, opts:split(Str)).

-define(_equal6b(V, E), ?_assertEqual(E, test_parse6(V))).
-define(_error6(V, E), ?_assertThrow({optp_error, E, _}, test_parse6(V))).

complex_with_tuple_test_() ->
    [?_error6("", need_command),
     ?_error6("-oo", need_command),
     ?_error6("-a test", invalid_option),
     ?_equal6b("cmd", [#smpl_tpl{}, "cmd", ?SMPL_TPL_C, none, []]),
     ?_equal6b("-u 0 -i 42 -o -s toto -f -o -u 1 cmd -a spam foo bar",
               [#smpl_tpl{a = {{int, 42},
                               {flag, true},
                               {list, [0, 1]}},
                          b = {{str, "toto"}, {count, 2}}},
                "cmd", {{atom, spam}}, none, ["foo", "bar"]]),
     ?_equal6b("--accum=0 --integer=42 --counter --string=toto "
                   "--flag --counter --accum=1 cmd --atom=spam foo bar",
                   [#smpl_tpl{a = {{int, 42},
                                   {flag, true},
                                   {list, [0, 1]}},
                              b = {{str, "toto"}, {count, 2}}},
                    "cmd", {{atom, spam}}, none, ["foo", "bar"]])
    ].

-define(SMPL_LST_A, [{int, 0}, {flag, false}, {list, []}]).
-define(SMPL_LST_B, [{str, undefined}, {count, 0}]).
-define(SMPL_LST_C, [{atom, undefined}]).
-record(smpl_lst, {a = ?SMPL_LST_A,
                   b = ?SMPL_LST_B}).

test_parse7(Str) ->
    Sub = new(?SMPL_LST_C,
              [iparam("a", "atom", atom, [1, 2])]),
    P = new(#smpl_lst{},
            [iparam("i", "integer", integer, [#smpl_lst.a, 1, 2]),
             iflag("f", "flag", [#smpl_lst.a, 2, 2]),
             iaccum("u", "accum", number, [#smpl_lst.a, 3, 2]),
             iparam("s", "string", string, [#smpl_lst.b, 1, 2]),
             icount("o", "counter", [#smpl_lst.b, 2, 2]),
             command("cmd", Sub)]),

    parse(P, opts:split(Str)).

-define(_equal7b(V, E), ?_assertEqual(E, test_parse7(V))).
-define(_error7(V, E), ?_assertThrow({optp_error, E, _}, test_parse7(V))).

complex_with_list_test_() ->
    [?_error7("", need_command),
     ?_error7("-oo", need_command),
     ?_error7("-a test", invalid_option),
     ?_equal7b("cmd", [#smpl_lst{}, "cmd", ?SMPL_LST_C, none, []]),
     ?_equal7b("-u 0 -i 42 -o -s toto -f -o -u 1 cmd -a spam foo bar",
               [#smpl_lst{a = [{int, 42},
                               {flag, true},
                               {list, [0, 1]}],
                          b = [{str, "toto"}, {count, 2}]},
                "cmd", [{atom, spam}], none, ["foo", "bar"]]),
     ?_equal7b("--accum=0 --integer=42 --counter --string=toto "
                   "--flag --counter --accum=1 cmd --atom=spam foo bar",
                   [#smpl_lst{a = [{int, 42},
                                   {flag, true},
                                   {list, [0, 1]}],
                              b = [{str, "toto"}, {count, 2}]},
                    "cmd", [{atom, spam}], none, ["foo", "bar"]])
    ].
