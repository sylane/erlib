%% ===========================================================================
%% @doc        GNU command line option parser.
%% @since      Apr 24, 2010
%% @version    1.0
%% @copyright  2009, Sebastien Merle <s.merle@gmail.com>
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

-module(optp).

-author('Sebastien Merle <s.merle@gmail.com>').

%TODO: Options without index
%TODO: Implement usage and help formating.


%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/2, new/3,
         chain/1, chain/2,
         parse/2,
         usage/1,
         help/1,
         version/1]).

-export([option/1,
         param/4, iparam/4, iparam/5,
         choice/5, ichoice/5, ichoice/6,
         accum/4, iaccum/4, iaccum/5,
         flag/3, iflag/3, iflag/4,
         count/3, icount/3, icount/4,
         command/1, command/2, command/3]).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

-record(opp, {options,
              optdefs = [],
              cmddefs = [],
              version = undefined,
              desc = undefined,
              usage = undefined,
              next = nil}).

-record(opt, {short = undefined,
              long = undefined,
              need_arg = false,
              value = true,
              type = string,
              choices = undefined,
              index = undefined,
              filters = [],
              modifiers = [],
              post = [],
              meta = undefined,
              desc = undefined}).

-record(cmd, {name,
              parser = nil,
              desc = undefined}).


%% ===================================================================
%% API Functions
%% ====================================================================

new(Defaults, Elements) ->
    new(Defaults, Elements, []).

new(Defaults, Elements, Options) ->
    Opp = #opp{options = Defaults,
               optdefs = [E || E <- Elements, is_option(E)],
               cmddefs = [E || E <- Elements, is_command(E)]},
    set_parser_options(Opp, Options).

chain(P1, P2) -> chain([P1, P2]).

chain([#opp{} = P]) -> P;
chain([#opp{} = P |Ps]) ->  P#opp{next = chain(Ps)}.

parse(Opp, Args) -> parse(Opp, Args, []).

usage(Opp) -> io:fwrite(get_usage(Opp)).

help(Opp) -> io:fwrite(get_help(Opp)).

version(Opp) -> io:fwrite(get_version(Opp)).

option(Options) ->
    set_options(#opt{}, Options, all).

iparam(ShortName, LongName, Type, Index) ->
    iparam(ShortName, LongName, Type, Index, []).

iparam(ShortName, LongName, Type, Index, Options) ->
    param(ShortName, LongName, Type, [{index, Index} |Options]).

param(ShortName, LongName, Type, Options) ->
    Opt = #opt{need_arg = true, type = Type,
               short = ShortName, long = LongName},
    Allowed = sets:from_list([index, modifier, filter, post, meta, desc]),
    set_options(Opt, Options, Allowed).

ichoice(ShortName, LongName, Type, Index, Choices) ->
    ichoice(ShortName, LongName, Type, Index, Choices, []).

ichoice(ShortName, LongName, Type, Index, Choices, Options) ->
    choice(ShortName, LongName, Type, Choices, [{index, Index} |Options]).

choice(ShortName, LongName, Type, Choices, Options) ->
    Opt = #opt{need_arg = true, type = Type,
               short = ShortName, long = LongName, choices=Choices},
    Allowed = sets:from_list([index, modifier, filter, post, meta, desc]),
    set_options(Opt, Options, Allowed).

iaccum(ShortName, LongName, Type, Index) ->
    iaccum(ShortName, LongName, Type, Index, []).

iaccum(ShortName, LongName, Type, Index, Options) ->
    accum(ShortName, LongName, Type, [{index, Index} |Options]).

accum(ShortName, LongName, Type, Options) ->
    Opt = #opt{need_arg = true, type = Type,
               short = ShortName, long = LongName,
               post = [fun lists:reverse/1]},
    Allowed = sets:from_list([index, modifier, filter, post, meta, desc]),
    OptWithMod = Options ++ [{modifier, fun(O, V) -> [V |O] end}],
    set_options(Opt, OptWithMod, Allowed).

iflag(ShortName, LongName, Index) ->
    iflag(ShortName, LongName, Index, []).

iflag(ShortName, LongName, Index, Options) ->
    flag(ShortName, LongName, [{index, Index} |Options]).

flag(ShortName, LongName, Options) ->
    Opt = #opt{short = ShortName, long = LongName, value = true},
    Allowed = sets:from_list([index, value, modifier, filter, post, desc]),
    set_options(Opt, Options, Allowed).

icount(ShortName, LongName, Index) ->
    icount(ShortName, LongName, Index, []).

icount(ShortName, LongName, Index, Options) ->
    count(ShortName, LongName, [{index, Index} |Options]).

count(ShortName, LongName, Options) ->
    Opt = #opt{short = ShortName, long = LongName,
               modifiers = [fun(O, V) -> O + V end], value = 1},
    Allowed = sets:from_list([index, value, modifier, filter, post, desc]),
    set_options(Opt, Options, Allowed).

command(Name) ->
    command(Name, []).

command(Name, #opp{} = Parser) -> command(Name, Parser, []);
command(Name, Options) ->
    set_command_options(#cmd{name = Name}, Options).

command(Name, #opp{} = Parser, Options) ->
    command(Name, [{parser, Parser} |Options]).


%% ===================================================================
%% Internal Functions
%% ====================================================================

set_parser_options(Opp, []) -> Opp;
set_parser_options(Opp, [{version, Version} | Tail]) ->
    set_parser_options(Opp#opp{version = Version}, Tail);
set_parser_options(Opp, [{usage, Usage} | Tail]) ->
    set_parser_options(Opp#opp{usage = Usage}, Tail);
set_parser_options(Opp, [{desc, Desc} | Tail]) ->
    set_parser_options(Opp#opp{desc = Desc}, Tail);
set_parser_options(_Opp, _Options) ->
    throw(badopt).

set_options(Opt, [], _Allowed) ->
    Opt#opt{modifiers = lists:reverse(Opt#opt.modifiers),
            post = lists:reverse(Opt#opt.post)};
set_options(Opt, [{Tag, Val} |Tail], all) ->
    set_option(Opt, Tag, Val, Tail, all, true);
set_options(Opt, [Tag |Tail], all) ->
    set_flag(Opt, Tag, Tail, all, true);
set_options(Opt, [{Tag, Val} |Tail], Allowed) ->
    set_option(Opt, Tag, Val, Tail, Allowed, sets:is_element(Tag, Allowed));
set_options(Opt, [Tag |Tail], Allowed) ->
    set_flag(Opt, Tag, Tail, Allowed, sets:is_element(Tag, Allowed)).

set_flag(Opt, need_arg, Tail, Allowed, true) ->
    set_options(Opt#opt{need_arg = true}, Tail, Allowed);
set_flag(_Opt, _Name, _Tail, _Allowed, _) ->
    throw(badopt).

set_option(Opt, short, Short, Tail, Allowed, true) ->
    set_options(Opt#opt{short = Short}, Tail, Allowed);
set_option(Opt, long, Long, Tail, Allowed, true) ->
    set_options(Opt#opt{long = Long}, Tail, Allowed);
set_option(Opt, index, Index, Tail, Allowed, true) ->
    set_options(Opt#opt{index = Index}, Tail, Allowed);
set_option(Opt, type, Type, Tail, Allowed, true) ->
    set_options(Opt#opt{type = Type}, Tail, Allowed);
set_option(Opt, value, Value, Tail, Allowed, true) ->
    set_options(Opt#opt{value = Value}, Tail, Allowed);
set_option(Opt, filter, Filter, Tail, Allowed, true) ->
    set_options(Opt#opt{filters = [Filter |Opt#opt.filters]}, Tail, Allowed);
set_option(Opt, modifier, Modifier, Tail, Allowed, true) ->
    Modifiers = [Modifier |Opt#opt.modifiers],
    set_options(Opt#opt{modifiers = Modifiers}, Tail, Allowed);
set_option(Opt, post, Post, Tail, Allowed, true) ->
    set_options(Opt#opt{post = [Post |Opt#opt.post]}, Tail, Allowed);
set_option(Opt, choices, Choices, Tail, Allowed, true) ->
    set_options(Opt#opt{choices = Choices}, Tail, Allowed);
set_option(Opt, meta, Meta, Tail, Allowed, true) ->
    set_options(Opt#opt{meta = Meta}, Tail, Allowed);
set_option(Opt, desc, Desc, Tail, Allowed, true) ->
    set_options(Opt#opt{desc = Desc}, Tail, Allowed);
set_option(_Opt, _Tag, _Val, _Tail, _Allowed, _) ->
    throw(badopt).

set_command_options(Cmd, []) -> Cmd;
set_command_options(Cmd, [{parser, Parser} |Tail]) ->
    set_command_options(Cmd#cmd{parser = Parser}, Tail);
set_command_options(Cmd, [{desc, Desc} |Tail]) ->
    set_command_options(Cmd#cmd{desc = Desc}, Tail);
set_command_options(_Cmd, _Options) ->
    throw(badopt).

get_usage(_Opp) ->  erlang:error(not_implemented).

get_help(_Opp) -> erlang:error(not_implemented).

get_version(#opp{desc = undefined, version = Version}) ->
    [io_lib:format("Version ~s~n", [Version])];

get_version(#opp{desc = Desc, version = Version}) ->
    [io_lib:format("~s - Version ~s~n", [Desc, Version])].

parse(P, [], R) -> parse_opt_finished(P, R, []);
parse(P, [A | As], R) -> parse_arg(P, A, As, R).

parse_arg(P, [], As, R) -> parse(P, As, R);
parse_arg(P, [$- | Os], As, R) -> parse_opt(P, Os, As, R);
parse_arg(P, C , As, R) -> parse_cmd(P, C, As, R).

parse_opt(P, "-", As, R) -> parse_opt_finished(P, R, As); % Double-hyphen
parse_opt(P, [$- | O], As, R) -> parse_lopt(P, O, As, R);
parse_opt(P, Os, As, R) -> parse_sopts(P, Os, As, R).

parse_opt_finished(#opp{cmddefs = []} = P, R, As) ->
    parse_finish(P, [none, pprocess_options(P) | R], As);
parse_opt_finished(P, _R, _T) -> error_cmd_needed(P).

parse_cmd(#opp{cmddefs = []} = P, C, As, R) ->
    parse_opt_finished(P, R, [C | As]);
parse_cmd(P, C, As, R) ->
    F = lists:keyfind(C, #cmd.name, P#opp.cmddefs),
    parse_cmd(P, F, C, As, R).

parse_cmd(#opp{next = nil} = P, false, C, _As, _R) ->
    error_invalid_cmd(P, C);
parse_cmd(#opp{next = Next}, false, C, As, R) ->
    parse(Next, [C |As], R); % Delegate to the next parser
parse_cmd(P, #cmd{parser = nil} = D, _C, As, R) ->
    parse_finish(P, [nil, D#cmd.name, pprocess_options(P) | R], As);
parse_cmd(P, D, _C, As, R) ->
    parse(D#cmd.parser, As, [D#cmd.name, pprocess_options(P) | R]).

parse_finish(#opp{next = nil}, R, As) ->
    lists:reverse([As |R]);
parse_finish(#opp{next = Next}, R, As) ->
    parse(Next, As, R).

parse_lopt(P, O, As, R) ->
    {ok, Rx} = re:compile("="),
    parse_lopt_split(P, re:split(O, Rx, [{return,list}, {parts, 2}]), As, R).

parse_lopt_split(P, [O | []], As, R) -> parse_lopt(P, O, none, As, R);
parse_lopt_split(P, [O, A], As, R) -> parse_lopt(P, O, A, As, R).

parse_lopt(P, O, A, As, R) ->
    F = lists:keyfind(O, #opt.long, P#opp.optdefs),
    parse_lopt(P, F, O, A, As, R).

parse_lopt(P, false, O, none, As, R) ->
    Opt = lists:flatten(["--", O]),
    unknown_opt(P, [Opt |As], R, Opt);
parse_lopt(P, false, O, A, As, R) ->
    Opt = lists:flatten(["--", O]),
    Arg = lists:flatten(["--", O, $=, A]),
    unknown_opt(P, [Arg |As], R, Opt);
parse_lopt(P, #opt{need_arg = true} = D, _O, none, _As, _R) ->
    error_arg_needed(P, D);
parse_lopt(P, #opt{need_arg = false} = D, _O, none, As, R) ->
    parse_flagmodifiers(P, D, [], As, R);
parse_lopt(P, #opt{need_arg = true} = D, _O, A, As, R) ->
    parse_param(P, D, A, As, R);
parse_lopt(P, D, _O, _A, _As, _R) -> error_arg_not_allowed(P, D).

parse_sopts(P, [], As, R) -> parse(P, As, R);
parse_sopts(P, [O | Os], As, R) -> parse_sopt(P, O, Os, As, R).

parse_sopt(P, O, Os, As, R) ->
    parse_sopt(P, lists:keyfind([O], #opt.short, P#opp.optdefs), O, Os, As, R).

parse_sopt(P, false, O, Os, As, R) ->
    unknown_opt(P, [[$-, O |Os] |As], R, [$-, O]);
parse_sopt(P, #opt{need_arg = true} = D, _O, [], As, R) ->
    parse_sopt_param(P, D, As, R);
parse_sopt(P, D, _O, Os, As, R) ->
    parse_flagmodifiers(P, D, Os, As, R).

parse_sopt_param(P, D, [], _R) -> error_arg_needed(P, D);
parse_sopt_param(P, D, [A | As], R) -> parse_param(P, D, A, As, R).

parse_param(P, #opt{type = string} = D, A, As, R) ->
    parse_preparg(P, D, As, R, A);
parse_param(P, #opt{type = number} = D, A, As, R) ->
    parse_nparam(P, D, As, R, A, string:to_float(string:strip(A)));
parse_param(P, #opt{type = integer} = D, A, As, R) ->
    parse_iparam(P, D, As, R, A, string:to_integer(string:strip(A)));
parse_param(P, #opt{type = atom} = D, A, As, R) ->
    parse_preparg(P, D, As, R, erlang:list_to_atom(A));
parse_param(P, #opt{type = binary} = D, A, As, R) ->
    parse_preparg(P, D, As, R, erlang:list_to_binary(A)).

parse_nparam(P, D, As, R, S, {error, no_float}) ->
    parse_iparam(P, D, As, R, S, string:to_integer(S));
parse_nparam(P, D, As, R, _S, {V, []}) ->
    parse_preparg(P, D, As, R, V);
parse_nparam(P, D, _As, _R, S, {_V, _}) -> % Have a garbage
    error_invalid_number(P, D, S).

parse_iparam(P, D, _As, _R, S, {error, no_integer}) ->
    error_invalid_int(P, D, S);
parse_iparam(P, D, As, R, _S, {V, []}) ->
    parse_preparg(P, D, As, R, V);
parse_iparam(P, D, _As, _R, S, {_V, _}) -> % Have a garbage
    error_invalid_int(P, D, S).

parse_preparg(P, #opt{choices = undefined} = D, As, R, V) ->
    parse_choices(P, D, As, R, V, true);
parse_preparg(P, #opt{choices = Choices} = D, As, R, V) ->
    parse_choices(P, D, As, R, V, lists:member(V, Choices)).

parse_choices(P, D, As, R, V, true) ->
    parse_argmodifiers(P, D, As, R, V);
parse_choices(P, D, _As, _R, V, false) ->
    error_invalid_choice(P, D, V).

parse_argmodifiers(P, D, As, R, V) ->
    parse_argmodifiers(P, D, As, R, V, D#opt.modifiers).

parse_argmodifiers(P, D, As, R, V, []) ->
    parse_setarg(P, D, As, R, V);
parse_argmodifiers(P, D, As, R, V, [F |Fs]) ->
    V2 = F(get_optval(P, D#opt.index), V),
    parse_argmodifiers(P, D, As, R, V2, Fs).

parse_setarg(P, D, As, R, V) ->
    parse(set_optval(P, D#opt.index, V), As, R).

parse_flagmodifiers(P, #opt{value = V} = D, Os, As, R) ->
    parse_flagmodifiers(P, D, Os, As, R, V, D#opt.modifiers).

parse_flagmodifiers(P, D, Os, As, R, V, []) ->
    parse_setflag(P, D, Os, As, R, V);
parse_flagmodifiers(P, D, Os, As, R, V, [F |Fs]) ->
    V2 = F(get_optval(P, D#opt.index), V),
    parse_flagmodifiers(P, D, Os, As, R, V2, Fs).

parse_setflag(P, D, Os, As, R, V) ->
    parse_sopts(set_optval(P, D#opt.index, V), Os, As, R).

pprocess_options(#opp{optdefs = Defs} = P) ->
    pprocess_options(P, Defs).

pprocess_options(P, []) -> P#opp.options;
pprocess_options(P, [#opt{post = Fs} = D |Ds]) ->
    pprocess_options(P, D, Ds, Fs).

pprocess_options(P, _D, Ds, []) -> pprocess_options(P, Ds);
pprocess_options(P, D, Ds, [F |Fs]) ->
    V = F(get_optval(P, D#opt.index)),
    pprocess_options(set_optval(P, D#opt.index, V), D, Ds, Fs).

get_optval(P, I) when is_integer(I) ->
    get_optval(P, [I]);
get_optval(P, Is) ->
    get_deep_option(P#opp.options, Is).

get_deep_option(T, [I |[]]) when is_tuple(T) ->
    element(I, T);
get_deep_option(T, [I |Is]) when is_tuple(T) ->
    get_deep_option(element(I, T), Is);
get_deep_option(T, [I |[]]) when is_list(T) ->
    lists:nth(I, T);
get_deep_option(T, [I |Is]) when is_list(T) ->
    get_deep_option(lists:nth(I, T), Is).

set_optval(P, I, V) when is_integer(I) ->
    set_optval(P, [I], V);
set_optval(P, Is, V) ->
    P#opp{options = set_deep_option(P#opp.options, Is, V)}.

set_deep_option(T, [I |[]], V) when is_tuple(T) ->
  setelement(I, T, V);
set_deep_option(T, [I |Is], V) when is_tuple(T) ->
  setelement(I, T, set_deep_option(element(I, T), Is, V));
set_deep_option(T, [I |[]], V) when is_list(T) ->
  set_list_element(I, T, V);
set_deep_option(T, [I |Is], V) when is_list(T) ->
  set_list_element(I, T, set_deep_option(lists:nth(I, T), Is, V)).

set_list_element(I, L, V) ->
  tuple_to_list(setelement(I, list_to_tuple(L), V)).

unknown_opt(#opp{cmddefs = [_|_]} = P, _As, _R, Desc) ->
    error_invalid_opt(P, Desc);
unknown_opt(#opp{next = nil} = P, _As, _R, Desc) ->
    error_invalid_opt(P, Desc);
unknown_opt(P, As, R, _Desc) ->
    parse_finish(P, [none, pprocess_options(P) | R], As).

error_cmd_needed(P) ->
    parse_failed(P, need_command, "No command specified").

error_invalid_cmd(P, C) ->
    Msg = io_lib:format("Invalid command ~s", [C]),
    parse_failed(P, invalid_command, Msg).

error_invalid_opt(P, Desc) ->
    Msg = io_lib:format("Invalid option ~s", [Desc]),
    parse_failed(P, invalid_option, Msg).

error_arg_needed(P, D) ->
    Msg = io_lib:format("Option ~s need an argument", [opt_desc(compact, D)]),
    parse_failed(P, need_argument, Msg).

error_arg_not_allowed(P, D) ->
    Msg = io_lib:format("Option ~s doesn't allow an argument",
                        [opt_desc(compact, D)]),
    parse_failed(P, argument_not_allowed, Msg).

error_invalid_int(P, D, V) ->
    Msg = io_lib:format("Invalid argument for option ~s; "
                            "~p is not a valid integer",
                            [opt_desc(compact, D), V]),
    parse_failed(P, invalid_argument, Msg).

error_invalid_number(P, D, V) ->
    Msg = io_lib:format("Invalid argument for option ~s; "
                            "~p is not a valid number",
                            [opt_desc(compact, D), V]),
    parse_failed(P, invalid_argument, Msg).

error_invalid_choice(P, D, V) ->
    Msg = io_lib:format("Invalid argument for option ~s; "
                            "the value should be ~s not ~p",
                            [opt_desc(compact, D), choice_desc(D), V]),
    parse_failed(P, invalid_argument, Msg).

parse_failed(_P, E, M) -> throw({optp_error, E, M}).

opt_desc(_, #opt{short = undefined, long = Long}) -> "--" ++ Long;
opt_desc(_, #opt{short = Short, long = undefined}) -> "-" ++ Short;
%% opt_desc(help, #opt{short = Short, long = Long}) ->
%%     io_lib:format("-~s, --~s", [Short, Long]);
opt_desc(compact, #opt{short = Short, long = Long}) ->
    io_lib:format("-~s/--~s", [Short, Long]).

choice_desc(#opt{choices = Choices}) ->
    choice_desc(Choices);
choice_desc(Choices) ->
    choice_desc([io_lib:format("~p", [V]) || V <- Choices], []).

choice_desc([], R) -> lists:flatten(lists:reverse(R));
choice_desc([C | Cs], []) -> choice_desc(Cs, [C]);
choice_desc([C], R) -> choice_desc([], [[" or ", C] | R]);
choice_desc([C | Cs], R) -> choice_desc(Cs, [[", ", C] | R]).

is_option(#opt{}) -> true;
is_option(_) -> false.

is_command(#cmd{}) -> true;
is_command(_) -> false.


%% ===================================================================
%% Internal Functions Unit Tests
%% ====================================================================

set_optval_tuple_test_() ->
    [?_assertEqual(set_optval(#opp{options = {0, 1, 2}}, 2, 123),
                   #opp{options = {0, 123, 2}}),
     ?_assertEqual(set_optval(#opp{options = {0, {0, 1, 2}, 2}}, [2, 2], 123),
                   #opp{options = {0, {0, 123, 2}, 2}}),
     ?_assertEqual(set_optval(#opp{options = {0, 1, {{0, 1, 2}, 1, 2}}},
                              [3, 1, 2], 123),
                   #opp{options = {0, 1, {{0, 123, 2}, 1, 2}}})
     ].

get_optval_tuple_test_() ->
    [?_assertEqual(get_optval(#opp{options = {0, 1, 2}}, 2), 1),
     ?_assertEqual(get_optval(#opp{options = {0, {3, 4, 5}, 2}}, [2, 2]), 4),
     ?_assertEqual(get_optval(#opp{options = {0, 1, {{6, 7, 6}, 4, 5}}},
                              [3, 1, 2]), 7)
     ].

set_optval_list_test_() ->
    [?_assertEqual(set_optval(#opp{options = [0, 1, 2]}, 2, 123),
                   #opp{options = [0, 123, 2]}),
     ?_assertEqual(set_optval(#opp{options = [0, [0, 1, 2], 2]}, [2, 2], 123),
                   #opp{options = [0, [0, 123, 2], 2]}),
     ?_assertEqual(set_optval(#opp{options = [0, 1, [[0, 1, 2], 1, 2]]},
                              [3, 1, 2], 123),
                   #opp{options = [0, 1, [[0, 123, 2], 1, 2]]})
     ].

get_optval_list_test_() ->
    [?_assertEqual(get_optval(#opp{options = [0, 1, 2]}, 2), 1),
     ?_assertEqual(get_optval(#opp{options = [0, [3, 4, 5], 2]}, [2, 2]), 4),
     ?_assertEqual(get_optval(#opp{options = [0, 1, [[6, 7, 6], 4, 5]]},
                              [3, 1, 2]), 7)
     ].

set_optval_mixed_test_() ->
    [?_assertEqual(set_optval(#opp{options = [0, 1, [[0, 1, 2], 1, 2]]},
                              [3, 1, 2], 123),
                   #opp{options = [0, 1, [[0, 123, 2], 1, 2]]}),
     ?_assertEqual(set_optval(#opp{options = [0, 1, {[0, 1, 2], 1, 2}]},
                              [3, 1, 2], 123),
                   #opp{options = [0, 1, {[0, 123, 2], 1, 2}]}),
     ?_assertEqual(set_optval(#opp{options = {0, 1, [{0, 1, 2}, 1, 2]}},
                              [3, 1, 2], 123),
                   #opp{options = {0, 1, [{0, 123, 2}, 1, 2]}})
     ].

get_optval_mixed_test_() ->
    [?_assertEqual(get_optval(#opp{options = [0, 1, {[6, 7, 6], 4, 5}]},
                              [3, 1, 2]), 7),
     ?_assertEqual(get_optval(#opp{options = {0, 1, [{6, 7, 6}, 4, 5]}},
                              [3, 1, 2]), 7)
     ].
