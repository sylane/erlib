%% ===========================================================================
%% @doc        Logging module the use erlang parse_transform to log context
%%             information like file, funtion, arity, and line number along
%%             with the log level, pid and time.
%%             A log category can be specified with the module attribute
%%             erlog_category or set at runtime for the active process.
%%             The category will be used in the futur to filter the log lines
%%             at runtime.
%%             A log name can be specified at runtime for the active process
%%             to differentiate the log entries.
%%             The module attribute erlog_level can be set to remove
%%             the log function calls at build time when the log level
%%             priority is higher than the specified one.
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
%%   * Neither the name of "eflusion" nor the names of its contributors may be
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

-module(erlog).
-author('Sebastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([parse_transform/2]).
-export([set_category/1, set_name/1]).

%% Compatibility exports
-export([log/1, log/2]).
-export([debug/1, debug/2]).
-export([info/1, info/2]).
-export([warn/1, warn/2]).
-export([error/1, error/2]).

%% Private exports
-export([publish/3, publish/4]).

%% --------------------------------------------------------------------
%% Constants
%% --------------------------------------------------------------------

-define(LOG_SEP, " ").
-define(LVL_FMT, "~-5s").
-define(PID_FMT, "~-10w").
-define(CAT_FMT, "~-12s").
-define(NAME_FMT, "~-12s").
-define(TIME_FMT, "~-24s").
-define(CTX_FMT, "(~s:~s/~w:~w)").
-define(CTX_FUN_FMT, "(~s:~s/~w:~w/~w:~w)").

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

-record(ctx, {category, level = log, file, module, line,
              function, arity, fun_index = 0, fun_arity = 0}).

%% --------------------------------------------------------------------
%% Global Specs
%% --------------------------------------------------------------------

-type log_level() :: log | debug | info | warn | error.


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Override the module log category for the active process.
%% The log category will be use in the future to filter log line at runtime.
-spec set_category(atom()) -> any().
%% --------------------------------------------------------------------
set_category(Category) ->
    put(erlog_category, Category).

%% --------------------------------------------------------------------
%% Set the logging name for the active process.
%% The log name apear in each log line after the logging category
%% and can be used to discriminate them with context information
%% like IP, username...
-spec set_name(atom()) -> any().
%% --------------------------------------------------------------------
set_name(Name) ->
    put(erlog_name, Name).

%% --------------------------------------------------------------------
%% Compatibility functions used when parse_transform has not been used.
%% --------------------------------------------------------------------
log(Msg) -> publish(log, Msg, none, none).
log(Msg, Vars) -> publish(log, Msg, Vars, none).
debug(Msg) -> publish(debug, Msg, none, none).
debug(Msg, Vars) -> publish(debug, Msg, Vars, none).
info(Msg) -> publish(info, Msg, none, none).
info(Msg, Vars) -> publish(info, Msg, Vars, none).
warn(Msg) -> publish(warn, Msg, none, none).
warn(Msg, Vars) -> publish(warn, Msg, Vars, none).
error(Msg) -> publish(error, Msg, none, none).
error(Msg, Vars) -> publish(error, Msg, Vars, none).

%% --------------------------------------------------------------------
%% Functions publish/3 calls are generated by the parse_transform.
-spec publish(log_level(), string(), #ctx{}) -> any().
%% --------------------------------------------------------------------
publish(Lvl, Msg, Ctx) -> console_logging(Lvl, Msg, none, Ctx).

%% --------------------------------------------------------------------
%% Functions publish/3 calls are generated by the parse_transform.
-spec publish(log_level(), string(), list(), #ctx{}) -> any().
%% --------------------------------------------------------------------
publish(Lvl, Msg, Vars, Ctx) -> console_logging(Lvl, Msg, Vars, Ctx).

%% --------------------------------------------------------------------
%% Parses a module AST and changes the remote calls to the erlog module's
%% function log, debug, info, war and error to call to the publish function
%% adding a context record that contains information like file, function
%% arity and line number.
%% --------------------------------------------------------------------
parse_transform(Ast, _Options) ->
    {NewAST, _NewCtx} = parse_ast(Ast, #ctx{}),
    NewAST.


%% ====================================================================
%% Local Functions
%% ====================================================================

lvl2lbl(log) -> "LOG";
lvl2lbl(debug) -> "DEBUG";
lvl2lbl(info) -> "INFO";
lvl2lbl(warn) -> "WARN";
lvl2lbl(error) -> "ERROR".

lvl2pri(log) -> 5;
lvl2pri(debug) -> 4;
lvl2pri(info) -> 3;
lvl2pri(warn) -> 2;
lvl2pri(error) -> 1.

console_logging(Lvl, Msg, Vars, Ctx) ->
    console_log_line(gen_log_line(Lvl, Msg, Vars, Ctx)).

console_log_line(Line) -> io:format("~s~n", [Line]).

tostr_ifdef(Val) ->
    case Val of
        undefined -> "";
        Any -> io_lib:format("~w", [Any])
    end.

get_default(Key, Default) ->
    case get(Key) of
        undefined -> Default;
        Any -> Any
    end.

gen_log_line(Lvl, Msg, Vars, Ctx) -> gen_log_level(Lvl, Msg, Vars, Ctx, []).

gen_log_level(Lvl, Msg, Vars, Ctx, Acc) ->
    NewAcc = [io_lib:format(?LVL_FMT, [lvl2lbl(Lvl)]) | Acc],
    gen_log_pid(Msg, Vars, Ctx, NewAcc).

gen_log_pid(Msg, Vars, Ctx, Acc) ->
    NewAcc = [io_lib:format(?PID_FMT, [self()]) | Acc],
    gen_log_category(Msg, Vars, Ctx, NewAcc).

gen_log_category(Msg, Vars, none, Acc) ->
    Category = tostr_ifdef(get(erlog_category)),
    NewAcc = [io_lib:format(?CAT_FMT, [Category]) | Acc],
    gen_log_name(Msg, Vars, none, NewAcc);
gen_log_category(Msg, Vars, Ctx, Acc) ->
    Category = tostr_ifdef(get_default(erlog_category, Ctx#ctx.category)),
    NewAcc = [io_lib:format(?CAT_FMT, [Category]) | Acc],
    gen_log_name(Msg, Vars, Ctx, NewAcc).

gen_log_name(Msg, Vars, Ctx, Acc) ->
    Name = tostr_ifdef(get(erlog_name)),
    NewAcc = [io_lib:format(?NAME_FMT, [Name]) | Acc],
    gen_log_time(Msg, Vars, Ctx, NewAcc).

gen_log_time(Msg, Vars, Ctx, Acc) ->
    NowStr = timelib:to_string(timelib:now()),
    NewAcc = [io_lib:format(?TIME_FMT, [NowStr]) | Acc],
    gen_log_msg(Msg, Vars, Ctx, NewAcc).

gen_log_msg(Msg, none, Ctx, Acc) ->
    gen_log_context(Ctx, [Msg | Acc]);
gen_log_msg(Msg, Vars, Ctx, Acc) ->
    gen_log_context(Ctx, [io_lib:format(Msg, Vars) | Acc]).

gen_log_context(none, Acc) ->
  gen_log_reverse(Acc, []);
gen_log_context(Ctx, Acc) when
  Ctx#ctx.fun_index > 0 ->
    CtxStr = io_lib:format(?CTX_FUN_FMT,
                           [Ctx#ctx.file, Ctx#ctx.function,
                            Ctx#ctx.arity, Ctx#ctx.fun_index,
                            Ctx#ctx.fun_arity, Ctx#ctx.line]),
    gen_log_reverse([CtxStr | Acc], []);
gen_log_context(Ctx, Acc) ->
    CtxStr = io_lib:format(?CTX_FMT,
                           [Ctx#ctx.file, Ctx#ctx.function,
                            Ctx#ctx.arity, Ctx#ctx.line]),
    gen_log_reverse([CtxStr | Acc], []).

gen_log_reverse([], Acc) -> Acc;
gen_log_reverse([H |T], []) -> gen_log_reverse(T, [H]);
gen_log_reverse([H |T], Acc) -> gen_log_reverse(T, [H, ?LOG_SEP |Acc]).

%% AST parse and transform

parse_ast(Ast, Ctx) ->
    parse_forms(Ast, [], Ctx).

parse_forms([], [], Ctx) ->
    {[{atom, 0, nop}], Ctx};
parse_forms([], Acc, Ctx) ->
    {lists:reverse(Acc), Ctx};
parse_forms([Form |Forms], Acc, Ctx) ->
    case parse_form(Form, Ctx) of
        {error, Error} -> parse_forms([], [{error, Error} |Acc], Ctx);
        {none, NewCtx} -> parse_forms(Forms, Acc, NewCtx);
        {NewForm, NewCtx} -> parse_forms(Forms, [NewForm |Acc], NewCtx)
    end.

parse_form({attribute, Line, module, Mod}, Ctx) ->
    {{attribute, Line, module, Mod}, Ctx#ctx{module = Mod}};
parse_form({attribute, Line, file, {File, Line}}, Ctx) ->
    AbsFile = pathlib:absname(File),
    {{attribute, Line, file, {File, Line}}, Ctx#ctx{file=AbsFile}};
parse_form({attribute, _Line, erlog_category, Category}, Ctx) ->
    {none, Ctx#ctx{category=Category}};
parse_form({attribute, _Line, erlog_level, Level}, Ctx) ->
    {none, Ctx#ctx{level=Level}};
parse_form({attribute, Line, Attr, Val}, Ctx) ->
    {{attribute,Line,Attr,Val}, Ctx};
parse_form({function, Line, Name, Arity, Clauses}, Ctx) ->
    case parse_function(Name, Arity, Clauses, Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewClauses, NewCtx} ->
            {{function, Line, Name, Arity, NewClauses}, NewCtx}
    end;
parse_form(Other, Ctx) ->
    {Other, Ctx}.

parse_function(Name, Arity, Clauses, Ctx) ->
    FunCtx = Ctx#ctx{function=Name, arity=Arity},
    case parse_clauses(Clauses, [], FunCtx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewClauses, NewCtx} ->
            {NewClauses, NewCtx#ctx{function=Ctx#ctx.function,
                                    arity=Ctx#ctx.arity,
                                    fun_index=undefined,
                                    fun_arity=undefined}}
    end.

parse_clauses([], [], Ctx) ->
    {none, Ctx};
parse_clauses([], Acc, Ctx) ->
    {lists:reverse(Acc), Ctx};
parse_clauses([Clause |Clauses], Acc, Ctx) ->
    case parse_clause(Clause, Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} ->
            parse_clauses(Clauses, Acc, NewCtx);
        {NewClause, NewCtx} ->
            parse_clauses(Clauses, [NewClause |Acc], NewCtx)
    end.

parse_clause({clause, Line, Head, Guards, Exprs}, Ctx) ->
    case parse_expressions(Exprs, [], Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} ->
            NewExprs = [{atom, Line, undefined}],
            {{clause, Line, Head, Guards, NewExprs}, NewCtx};
        {NewExprs, NewCtx} ->
            {{clause, Line, Head, Guards, NewExprs}, NewCtx}
    end.

parse_expressions([], [], Ctx) ->
    {none, Ctx};
parse_expressions([], Acc, Ctx) ->
    {lists:reverse(Acc), Ctx};
parse_expressions([Expr |Exprs], Acc, Ctx) ->
    case parse_expression(Expr, Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} ->
            parse_expressions(Exprs, Acc, NewCtx);
        {NewExpr, NewCtx} ->
            parse_expressions(Exprs, [NewExpr |Acc], NewCtx)
    end.

parse_expression({block, Line, Exprs}, Ctx) ->
    case parse_expressions(Exprs, [], Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewExprs, NewCtx} ->
            {{block, Line, NewExprs}, NewCtx}
    end;
parse_expression({'if', Line, Clauses}, Ctx) ->
    case parse_clauses(Clauses, [], Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewClauses, NewCtx} ->
            {{'if', Line, NewClauses}, NewCtx}
    end;
parse_expression({'case', Line, Expr, Clauses}, Ctx) ->
    case parse_clauses(Clauses, [], Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewClauses, NewCtx} ->
            {{'case', Line, Expr, NewClauses}, NewCtx}
    end;
parse_expression({'receive', Line, Clauses}, Ctx) ->
    case parse_clauses(Clauses, [], Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewClauses, NewCtx} ->
            {{'receive', Line, NewClauses}, NewCtx}
    end;
parse_expression({'receive', Line, Clauses, To, ToExpr}, Ctx0) ->
    case parse_clauses(Clauses, [], Ctx0) of
        {error, Error} -> {error, Error};
        {none, Ctx1} -> {none, Ctx1};
        {NewClauses, Ctx1} ->
            case parse_expression(ToExpr, Ctx1) of
                {error, Error} -> {error, Error};
                {none, Ctx2} ->
                    {{'receive', Line, NewClauses}, Ctx2};
                {NewToExpr, Ctx2} ->
                    {{'reveive', Line, NewClauses, To, NewToExpr}, Ctx2}
            end
    end;
parse_expression({'try', Line, Try, Case, Catch, After}, Ctx) ->
    parse_try(Line, Try, Case, Catch, After, Ctx);
parse_expression({'fun', Line, Body}, Ctx) ->
    case Body of
        {clauses, Clauses} ->
            FunCtx = Ctx#ctx{fun_index=Ctx#ctx.fun_index+1},
            {NewClauses, NewCtx} = parse_funclauses(Clauses, [], FunCtx),
            ResCtx = NewCtx#ctx{fun_index=Ctx#ctx.fun_index},
            {{'fun', Line, {clauses, NewClauses}}, ResCtx};
        {function, F, A} ->
            {{'fun', Line, {function, F, A}}, Ctx};
        {function, M, F, A} ->
            {{'fun', Line, {function, M, F, A}}, Ctx}
    end;
parse_expression({call, Line, CallDef, Args}, Ctx) ->
    case parse_call(CallDef, Args, Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewDef, NewArgs, NewCtx} ->
            {{call, Line, NewDef, NewArgs}, NewCtx}
    end;
parse_expression({match, Line, Pat, Expr}, Ctx) ->
    case parse_expression(Expr, Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewExpr, NewCtx} ->
            {{match, Line, Pat, NewExpr}, NewCtx}
    end;
parse_expression(Any, Ctx) ->
    {Any, Ctx}.

parse_try(Line, Try, Case, Catch, After, Ctx) ->
    case parse_expressions(Try, [], Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} ->
            parse_try_case(Line, none, Case, Catch, After, NewCtx);
        {NewTry, NewCtx} ->
            parse_try_case(Line, NewTry, Case, Catch, After, NewCtx)
    end.

parse_try_case(Line, Try, Case, Catch, After, Ctx) ->
    case parse_clauses(Case, [], Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} ->
            parse_try_catch(Line, Try, none, Catch, After, NewCtx);
        {NewCase, NewCtx} ->
            parse_try_catch(Line, Try, NewCase, Catch, After, NewCtx)
    end.

parse_try_catch(Line, Try, Case, Catch, After, Ctx) ->
    case parse_clauses(Catch, [], Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} ->
            parse_try_after(Line, Try, Case, none, After, NewCtx);
        {NewCatch, NewCtx} ->
            parse_try_after(Line, Try, Case, NewCatch, After, NewCtx)
    end.

parse_try_after(Line, Try, Case, Catch, After, Ctx) ->
    case parse_expressions(After, [], Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} ->
            parse_try_final(Line, Try, Case, Catch, none, NewCtx);
        {NewAfter, NewCtx} ->
            parse_try_final(Line, Try, Case, Catch, NewAfter, NewCtx)
    end.

parse_try_final(_Line, none, _Case, _Catch, _After, Ctx) ->
    {none, Ctx};
parse_try_final(Line, Try, Case, none, none, Ctx) ->
    {{'case', Line, Try, Case}, Ctx};
parse_try_final(Line, Try, none, Catch, none, Ctx) ->
    {{'try', Line, Try, [], Catch, []}, Ctx};
parse_try_final(Line, Try, none, none, After, Ctx) ->
    {{'try', Line, Try, [], [], After}, Ctx};
parse_try_final(Line, Try, Case, Catch, After, Ctx) ->
    {{'try', Line, Try, Case, Catch, After}, Ctx}.

parse_call({remote, L1, {atom, L2, erlog}, {atom, L3, Fun}}, Args, Ctx) ->
    case erlog_call(Fun, Args, Ctx#ctx{line=L3}) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewFun, NewArgs, NewCtx} ->
            {{remote, L1, {atom, L2, erlog},
              {atom, L3, NewFun}}, NewArgs, NewCtx}
    end;
parse_call(Any, Args, Ctx) ->
    {Any, Args, Ctx}.

parse_funclauses([], [], Ctx) ->
    {none, Ctx};
parse_funclauses([], Acc, Ctx) ->
    {lists:reverse(Acc), Ctx};
parse_funclauses([Clause |Clauses], Acc, Ctx) ->
    case parse_funclause(Clause, Ctx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewClause, NewCtx} ->
            parse_funclauses(Clauses, [NewClause |Acc], NewCtx)
    end.

parse_funclause({clause, Line, Head, Guards, Exprs}, Ctx) ->
    FunCtx = Ctx#ctx{fun_arity = length(Head)},
    case parse_expressions(Exprs, [], FunCtx) of
            {error, Error} -> {error, Error};
            {none, NewCtx} ->
                {none, NewCtx#ctx{fun_arity = Ctx#ctx.fun_arity}};
            {NewExprs, NewCtx} ->
                RevCtx = NewCtx#ctx{fun_arity = Ctx#ctx.fun_arity},
                {{clause, Line, Head, Guards, NewExprs}, RevCtx }
    end.

erlog_call(Fun, Args, Ctx) when
  Fun =:= log; Fun =:= debug; Fun =:= info; Fun =:= warn; Fun =:= error ->
	LvlDiff = lvl2pri(Ctx#ctx.level) - lvl2pri(Fun),
    if LvlDiff >= 0 -> erlog_publish(Fun, Args, Ctx);
       true -> {none, Ctx}
    end;
erlog_call(Fun, Args, Ctx) when
  Fun =:= set_name; Fun =:= set_category ->
    {Fun, Args, Ctx};
erlog_call(publish, _Args, #ctx{line=Line}) ->
    Msg = "Function erlog:publish should not be called directly",
    {error, {Line, erl_parse, [Msg]}};
erlog_call(Fun, _Args, #ctx{line=Line}) ->
    Msg = io_lib:format("Invalid logging category: '~w'", [Fun]),
    {error, {Line, erl_parse, [Msg]}}.

erlog_publish(Fun, [Msg], #ctx{line=Line} = Ctx) ->
    {publish, [{atom, Line, Fun}, Msg, context_ast(Line, Ctx)], Ctx};
erlog_publish(Fun, [Msg, Vars], #ctx{line=Line} = Ctx) ->
    {publish, [{atom, Line, Fun}, Msg, Vars, context_ast(Line, Ctx)], Ctx};
erlog_publish(_Fun, _Args, #ctx{line=Line}) ->
    {error, {Line, erl_parse, ["Invalid logging arguments"]}}.

context_ast(Line, Ctx) ->
    {tuple, Line, [{atom, Line, ctx},
                   {atom, Line, Ctx#ctx.category},
                   {atom, Line, Ctx#ctx.level},
                   {string, Line, Ctx#ctx.file},
                   {atom, Line, Ctx#ctx.module},
                   {integer, Line, Ctx#ctx.line},
                   {atom, Line, Ctx#ctx.function},
                   {integer, Line, Ctx#ctx.arity},
                   {integer, Line, Ctx#ctx.fun_index},
                   {integer, Line, Ctx#ctx.fun_arity}]}.
