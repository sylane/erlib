%% ===========================================================================
%% @doc        Erlog logging process.
%% @since      Apr 17, 2010
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

-module(erlog_logger).

-author('Sebastien Merle <s.merle@gmail.com>').

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("erlog.hrl").

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% Startup exports
-export([start_link/0]).

%% Behaviour gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% --------------------------------------------------------------------
%% Constants
%% --------------------------------------------------------------------

-define(LOG_FSEP, " ").
-define(LOG_LSEP, "\n").
-define(LOG_PCHAR, $ ).
-define(LVL_FMT, "~-5s").
-define(PID_FMT, "~-10w").
-define(CAT_FMT, "~-12s").
-define(NAME_FMT, "~-12s").
-define(TIME_FMT, "~-26s").
-define(CTX_FMT, "(~s:~s/~w:~w)").
-define(CTX_FUN_FMT, "(~s:~s/~w:~w/~w:~w)").


%% ====================================================================
%% Startup Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Starts and links an erlog logger process.

start_link() ->
    gen_server:start_link({local, erlog}, ?MODULE, {}, []).


%% ====================================================================
%% Behaviour gen_server Callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Called whenever the logger is started.

init({}) ->
    {ok, none}.


%% --------------------------------------------------------------------
%% @doc Called when receiving a request sent using gen_server:call.

handle_call(_Request, _From, _State) ->
    erlang:error(not_expected).


%% --------------------------------------------------------------------
%% @doc Called when receiving a request sent using gen_server:cast.

handle_cast({log, #erlog_entry{} = Entry}, State) ->
    ok = console_logging(Entry),
    {noreply, State};
handle_cast(_Msg, _State) ->
    erlang:error(not_expected).


%% --------------------------------------------------------------------
%% @doc Called when receiving any other messages.

handle_info(_Info, _State) ->
    erlang:error(not_expected).


%% --------------------------------------------------------------------
%% @doc Called when the logger process is about to termintes.

terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% @doc Called when the logger process should update its internal state.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal Functions
%% ====================================================================

console_logging(Entry) ->
    {Headers, Body, Footers} = gen_log_line(Entry),
    console_log_line(Headers, Body, Footers).

console_log_line(Headers, [Line], Footers) ->
    HeaderStr = string:join(Headers, ?LOG_FSEP),
    FooterStr = string:join(Footers, ?LOG_FSEP),
    io:fwrite([HeaderStr, ?LOG_FSEP, Line, ?LOG_FSEP, FooterStr, ?LOG_LSEP]);
console_log_line(Headers, Body, Footers) ->
    HeaderJoined = string:join(Headers, ?LOG_FSEP),
    HeaderStr = lists:flatten(HeaderJoined, ?LOG_FSEP),
    FooterStr = string:join(Footers, ?LOG_FSEP),
    Prefixes = [HeaderStr, lists:duplicate(length(HeaderStr), ?LOG_PCHAR)],
    BodyJoined = join_lines(Body, ?LOG_LSEP, Prefixes),
    io:fwrite([BodyJoined, ?LOG_FSEP, FooterStr, ?LOG_LSEP]).

join_lines(Lines, Sep, Prefixes) ->
    join_lines(Lines, [], Sep, Prefixes).

join_lines([Line], Acc, _Sep, [Prefix]) ->
    lists:reverse(Acc, [Prefix, Line]);
join_lines([Line], Acc, _Sep, [Prefix |_Prefixes]) ->
    lists:reverse(Acc, [Prefix, Line]);
join_lines([Line |Lines], Acc, Sep, [Prefix]) ->
    join_lines(Lines, [Sep, Line, Prefix |Acc], Sep, [Prefix]);
join_lines([Line |Lines], Acc, Sep, [Prefix |Prefixes]) ->
    join_lines(Lines, [Sep, Line, Prefix |Acc], Sep, Prefixes).

tostr_ifdef(undefined) -> "";
tostr_ifdef(Value) -> io_lib:format("~w", [Value]).

default_ifundef(undefined, Default) -> Default;
default_ifundef(Value, _Default) -> Value.

gen_log_line(Entry) ->
    gen_log_level(Entry, [], [], []).

gen_log_level(#erlog_entry{level = Lvl} = Entry, Headers, Body, Footers) ->
    NewHeaders = [io_lib:format(?LVL_FMT, [erlog:lvl2lbl(Lvl)]) | Headers],
    gen_log_pid(Entry, NewHeaders, Body, Footers).

gen_log_pid(#erlog_entry{pid = Pid} = Entry, Headers, Body, Footers) ->
    NewHeaders = [io_lib:format(?PID_FMT, [Pid]) | Headers],
    gen_log_category(Entry, NewHeaders, Body, Footers).

gen_log_category(#erlog_entry{proc_cat = ProcCat, ctx = undefined} = Entry,
                 Headers, Body, Footers) ->
    Category = tostr_ifdef(ProcCat),
    NewHeaders = [io_lib:format(?CAT_FMT, [Category]) | Headers],
    gen_log_name(Entry, NewHeaders, Body, Footers);
gen_log_category(#erlog_entry{proc_cat = ProcCat, ctx = Ctx} = Entry,
                 Headers, Body, Footers) ->
    Category = tostr_ifdef(default_ifundef(ProcCat, Ctx#erlog_ctx.category)),
    NewHeaders = [io_lib:format(?CAT_FMT, [Category]) | Headers],
    gen_log_name(Entry, NewHeaders, Body, Footers).

gen_log_name(#erlog_entry{proc_name = Name} = Entry, Headers, Body, Footers) ->
    NameStr = tostr_ifdef(Name),
    NewHeaders = [io_lib:format(?NAME_FMT, [NameStr]) | Headers],
    gen_log_time(Entry, NewHeaders, Body, Footers).

gen_log_time(#erlog_entry{log_time = Time} = Entry, Headers, Body, Footers) ->
    NowStr = timelib:to_string(Time),
    NewHeaders = [io_lib:format(?TIME_FMT, [NowStr]) | Headers],
    gen_log_msg(Entry, NewHeaders, Body, Footers).

gen_log_msg(#erlog_entry{msg = Msg, vars = undefined} = Entry,
            Headers, Body, Footers) ->
    NewBody = lists:reverse(re:split(Msg, "\n"), Body),
    gen_log_context(Entry, Headers, NewBody, Footers);
gen_log_msg(#erlog_entry{msg = Msg, vars = Vars} = Entry,
            Headers, Body, Footers) ->
    NewMsg = try io_lib:format(Msg, Vars)
             catch error:badarg ->
                       io_lib:format("BAD FORMAT: \"~s\" / ~w", [Msg, Vars])
             end,
    NewBody = lists:reverse(re:split(NewMsg, "\n"), Body),
    gen_log_context(Entry, Headers, NewBody, Footers).

gen_log_context(#erlog_entry{ctx = undefined} = Entry,
                Headers, Body, Footers) ->
  gen_log_finish(Entry, Headers, Body, Footers);
gen_log_context(#erlog_entry{ctx = Ctx} = Entry,
                Headers, Body, Footers) when
  Ctx#erlog_ctx.fun_index > 0 ->
    CtxStr = io_lib:format(?CTX_FUN_FMT,
                           [Ctx#erlog_ctx.file, Ctx#erlog_ctx.function,
                            Ctx#erlog_ctx.arity, Ctx#erlog_ctx.fun_index,
                            Ctx#erlog_ctx.fun_arity, Ctx#erlog_ctx.line]),
    gen_log_finish(Entry, Headers, Body, [CtxStr |Footers]);
gen_log_context(#erlog_entry{ctx = Ctx} = Entry,
                Headers, Body, Footers) ->
    CtxStr = io_lib:format(?CTX_FMT,
                           [Ctx#erlog_ctx.module, Ctx#erlog_ctx.function,
                            Ctx#erlog_ctx.arity, Ctx#erlog_ctx.line]),
    gen_log_finish(Entry, Headers, Body, [CtxStr |Footers]).

gen_log_finish(_Entry, Headers, Body, Footers) ->
    {lists:reverse(Headers), lists:reverse(Body), lists:reverse(Footers)}.
