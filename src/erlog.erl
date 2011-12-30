%% ===========================================================================
%% @doc        Erlang line logger.
%%             Uses erlang parse_transform to log context information like
%%             file, funtion, arity, and line number along with the log level,
%%             pid and time.
%%             A log category can be specified with the module attribute
%%             erlog_category or set at runtime for the active process.
%%             The category will be used in the futur to filter the log lines
%%             at runtime.
%%             A log name can be specified at runtime for the active process
%%             to differentiate the log entries.
%%             The module attribute erlog_max_level can be set to remove
%%             the log function calls at build time when the log level
%%             is higher than the specified one.
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

-module(erlog).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(application).

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-define(NO_ERLOG_PARSE_TRANSFORM, true).
-include("erlog.hrl").

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([start/0,
         stop/0,
         set_category/1,
         set_name/1,
         lvl2lbl/1,
         lvl2num/1]).

%% Behaviour application callbacks
-export([start/2,
         stop/1]).

%% Compatibility exports
-export([log/1, log/2]).
-export([debug/1, debug/2]).
-export([info/1, info/2]).
-export([warn/1, warn/2]).
-export([error/1, error/2]).

%% Private exports
-export([parse_transform/2,
         publish/3, publish/4]).


%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Starts the erlog application.
-spec start() -> ok | {error, Reason::term()}.
%% --------------------------------------------------------------------
start() -> application:start(erlog).

%% --------------------------------------------------------------------
%% Stops the erlog application.
-spec stop() -> ok | {error, Reason::term()}.
%% --------------------------------------------------------------------
stop() -> application:stop(erlog).

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
%% Gives a string label for the specified logging level.
%% --------------------------------------------------------------------
lvl2lbl(log) -> "LOG";
lvl2lbl(debug) -> "DEBUG";
lvl2lbl(info) -> "INFO";
lvl2lbl(warn) -> "WARN";
lvl2lbl(error) -> "ERROR";
lvl2lbl(none) -> "NONE".

%% --------------------------------------------------------------------
%% Gives the logging level as an integer.
%% --------------------------------------------------------------------
lvl2num(log) -> 5;
lvl2num(debug) -> 4;
lvl2num(info) -> 3;
lvl2num(warn) -> 2;
lvl2num(error) -> 1;
lvl2num(none) -> 0.

%% --------------------------------------------------------------------
%% Compatibility functions used when parse_transform has not been used.
%% --------------------------------------------------------------------
log(Msg) -> publish(log, Msg, undefined).
log(Msg, Vars) -> publish(log, Msg, Vars, undefined).
debug(Msg) -> publish(debug, Msg, undefined).
debug(Msg, Vars) -> publish(debug, Msg, Vars, undefined).
info(Msg) -> publish(info, Msg, undefined).
info(Msg, Vars) -> publish(info, Msg, Vars, undefined).
warn(Msg) -> publish(warn, Msg, undefined).
warn(Msg, Vars) -> publish(warn, Msg, Vars, undefined).
error(Msg) -> publish(error, Msg, undefined).
error(Msg, Vars) -> publish(error, Msg, Vars, undefined).

%% --------------------------------------------------------------------
%% Functions publish/3 calls are generated by the parse_transform.
-spec publish(log_level(), string(), #erlog_ctx{} | undefined) -> any().
%% --------------------------------------------------------------------
publish(Lvl, Msg, Ctx) ->
    publish(Lvl, Msg, undefined, Ctx).

%% --------------------------------------------------------------------
%% Functions publish/3 calls are generated by the parse_transform.
-spec publish(log_level(), string(), list() | undefined,
			  #erlog_ctx{} | undefined) -> any().
%% --------------------------------------------------------------------
publish(Lvl, Msg, Vars, Ctx) ->
    Entry = #erlog_entry{level = Lvl,
                         pid = self(),
                         proc_cat = get(erlog_category),
                         proc_name = get(erlog_name),
                         log_time = timelib:now(),
                         msg = Msg,
                         vars = Vars,
                         ctx = Ctx},
    gen_server:cast(erlog, {log, Entry}).

%% --------------------------------------------------------------------
%% Parses a module AST and changes the remote calls to the erlog module's
%% function log, debug, info, war and error to call to the publish function
%% adding a context record that contains information like file, function
%% arity and line number.
%% --------------------------------------------------------------------
parse_transform(Ast, _Options) ->
  {NewAst, _NewCtx} = parse_ast(Ast, #erlog_ctx{}),
  NewAst.


%% ====================================================================
%% Behaviour application Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Called whenever the application is started.
%% --------------------------------------------------------------------
start(_Type, []) -> erlog_sup:start_link().

%% --------------------------------------------------------------------
%% Called whenever the application has stopped.
%% --------------------------------------------------------------------
stop(_State) -> ok.


%% ====================================================================
%% Local Functions
%% ====================================================================

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
    {{attribute, Line, module, Mod}, Ctx#erlog_ctx{module = Mod}};
parse_form({attribute, Line, file, {File, Line}}, Ctx) ->
    AbsFile = pathlib:absname(File),
    {{attribute, Line, file, {File, Line}}, Ctx#erlog_ctx{file=AbsFile}};
parse_form({attribute, _Line, erlog_category, Category}, Ctx) ->
    {none, Ctx#erlog_ctx{category=Category}};
parse_form({attribute, _Line, erlog_max_level, Level},
           #erlog_ctx{max_level = MaxLevel} = Ctx) when
  Level =:= log; Level =:= debug; Level =:= info;
  Level =:= warn; Level =:= error; Level =:= none ->
    CurrMaxPri = lvl2num(MaxLevel),
    NewPri = lvl2num(Level),
    if NewPri < CurrMaxPri -> {none, Ctx#erlog_ctx{max_level=Level}};
       true -> {none, Ctx}
    end;
parse_form({attribute, Line, erlog_max_level, L}, _Ctx) ->
    Msg = io_lib:format("Invalid maximum logging level: ~w", [L]),
    {error, {Line, erl_parse, [Msg]}};
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
    FunCtx = Ctx#erlog_ctx{function=Name, arity=Arity},
    case parse_clauses(Clauses, [], FunCtx) of
        {error, Error} -> {error, Error};
        {none, NewCtx} -> {none, NewCtx};
        {NewClauses, NewCtx} ->
            {NewClauses, NewCtx#erlog_ctx{function=Ctx#erlog_ctx.function,
                                    arity=Ctx#erlog_ctx.arity,
                                    fun_index=0,
                                    fun_arity=0}}
    end.

parse_clauses([], [], Ctx) ->
    {none, Ctx};
parse_clauses([], Acc, Ctx) ->
    {lists:reverse(Acc), Ctx};
parse_clauses([Clause |Clauses], Acc, Ctx) ->
    case parse_clause(Clause, Ctx) of
        {error, Error} -> {error, Error};
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
            FunCtx = Ctx#erlog_ctx{fun_index=Ctx#erlog_ctx.fun_index+1},
            {NewClauses, NewCtx} = parse_funclauses(Clauses, [], FunCtx),
            ResCtx = NewCtx#erlog_ctx{fun_index=Ctx#erlog_ctx.fun_index},
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
parse_try_final(Line, Try, Case, Catch, none, Ctx) ->
    {{'try', Line, Try, Case, Catch, []}, Ctx};
parse_try_final(Line, Try, Case, Catch, After, Ctx) ->
    {{'try', Line, Try, Case, Catch, After}, Ctx}.

parse_call({remote, L1, {atom, L2, erlog}, {atom, L3, Fun}}, Args, Ctx) ->
    case erlog_call(Fun, Args, Ctx#erlog_ctx{line=L3}) of
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
    FunCtx = Ctx#erlog_ctx{fun_arity = length(Head)},
    case parse_expressions(Exprs, [], FunCtx) of
            {error, Error} -> {error, Error};
            {none, NewCtx} ->
                {none, NewCtx#erlog_ctx{fun_arity = Ctx#erlog_ctx.fun_arity}};
            {NewExprs, NewCtx} ->
                RevCtx = NewCtx#erlog_ctx{fun_arity = Ctx#erlog_ctx.fun_arity},
                {{clause, Line, Head, Guards, NewExprs}, RevCtx }
    end.

erlog_call(Fun, Args, Ctx) when
  Fun =:= log; Fun =:= debug; Fun =:= info; Fun =:= warn; Fun =:= error ->
  LvlDiff = lvl2num(Ctx#erlog_ctx.max_level) - lvl2num(Fun),
    if LvlDiff >= 0 -> erlog_publish(Fun, Args, Ctx);
       true -> {none, Ctx}
    end;
erlog_call(Fun, Args, Ctx) when
  Fun =:= set_name; Fun =:= set_category;
  Fun =:= start; Fun =:= stop;
  Fun =:= lvl2lbl; Fun =:= lvl2pri ->
    {Fun, Args, Ctx};
erlog_call(publish, _Args, #erlog_ctx{line=Line}) ->
    Msg = "Function erlog:publish should not be called directly",
    {error, {Line, erl_parse, [Msg]}};
erlog_call(Fun, _Args, #erlog_ctx{line=Line}) ->
    Msg = io_lib:format("Invalid logging category: '~w'", [Fun]),
    {error, {Line, erl_parse, [Msg]}}.

erlog_publish(Fun, [Msg], #erlog_ctx{line=Line} = Ctx) ->
    {publish, [{atom, Line, Fun}, Msg, context_ast(Line, Ctx)], Ctx};
erlog_publish(Fun, [Msg, Vars], #erlog_ctx{line=Line} = Ctx) ->
    {publish, [{atom, Line, Fun}, Msg, Vars, context_ast(Line, Ctx)], Ctx};
erlog_publish(_Fun, _Args, #erlog_ctx{line=Line}) ->
    {error, {Line, erl_parse, ["Invalid logging arguments"]}}.

context_ast(Line, Ctx) ->
    {tuple, Line, [{atom, Line, erlog_ctx},
                   {atom, Line, Ctx#erlog_ctx.category},
                   {atom, Line, Ctx#erlog_ctx.max_level},
                   {string, Line, Ctx#erlog_ctx.file},
                   {atom, Line, Ctx#erlog_ctx.module},
                   {integer, Line, Ctx#erlog_ctx.line},
                   {atom, Line, Ctx#erlog_ctx.function},
                   {integer, Line, Ctx#erlog_ctx.arity},
                   {integer, Line, Ctx#erlog_ctx.fun_index},
                   {integer, Line, Ctx#erlog_ctx.fun_arity}]}.
