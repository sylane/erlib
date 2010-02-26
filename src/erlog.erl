%% ===========================================================================
%% @doc        TODO: Add module erlog documentation.
%% @since      Nov 28, 2009
%% @version    1.0
%% @copyright  (c) 2009, Sébastien Merle <s.merle@gmail.com>
%% @authors    Sébastien Merle <s.merle@gmail.com>
%% @end
%%
%% Copyright (c) 2009, Sébastien Merle <s.merle@gmail.com>
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
-author('Sébastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([tmp/2]).
-export([log/1, log/2, log/3, log/4]).
-export([debug/1, debug/2, debug/3, debug/4]).
-export([info/1, info/2, info/3, info/4]).
-export([warn/1, warn/2, warn/3, warn/4]).
-export([error/1, error/2, error/3, error/4]).

%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% TODO: Add description of function tmp/2
%% --------------------------------------------------------------------
tmp(Tag, Values) when is_list(Values) ->
    Tmpl = string:join(["~s" |lists:duplicate(length(Values), "~p")], " "),
    Vals = [lists:duplicate(10, Tag) |Values],
    log_line(io_lib:format(Tmpl, Vals)).

%% --------------------------------------------------------------------
%% TODO: Add description of function log/1, log/2, log/3, log/4
%% --------------------------------------------------------------------
log(Msg) -> console_logging(log, Msg, none, unknown, unknown).
log(Msg, Vars) -> console_logging(log, Msg, Vars, unknown, unknown).
log(Msg, File, Line) -> console_logging(log, Msg, none, File, Line).
log(Msg, Vars, File, Line) -> console_logging(log, Msg, Vars, File, Line).

%% --------------------------------------------------------------------
%% TODO: Add description of function debug/1, debug/2, debug/3, debug/4
%% --------------------------------------------------------------------
debug(Msg) -> console_logging(debug, Msg, none, unknown, unknown).
debug(Msg, Vars) -> console_logging(debug, Msg, Vars, unknown, unknown).
debug(Msg, File, Line) -> console_logging(debug, Msg, none, File, Line).
debug(Msg, Vars, File, Line) -> console_logging(debug, Msg, Vars, File, Line).

%% --------------------------------------------------------------------
%% TODO: Add description of function info/1, info/2, info/3, info/4
%% --------------------------------------------------------------------
info(Msg) -> console_logging(info, Msg, none, unknown, unknown).
info(Msg, Vars) -> console_logging(info, Msg, Vars, unknown, unknown).
info(Msg, File, Line) -> console_logging(info, Msg, none, File, Line).
info(Msg, Vars, File, Line) -> console_logging(info, Msg, Vars, File, Line).

%% --------------------------------------------------------------------
%% TODO: Add description of function warn/1, warn/2, warn/3, warn/4
%% --------------------------------------------------------------------
warn(Msg) -> console_logging(warn, Msg, none, unknown, unknown).
warn(Msg, Vars) -> console_logging(warn, Msg, Vars, unknown, unknown).
warn(Msg, File, Line) -> console_logging(warn, Msg, none, File, Line).
warn(Msg, Vars, File, Line) -> console_logging(warn, Msg, Vars, File, Line).

%% --------------------------------------------------------------------
%% TODO: Add description of function error/1, error/2, error/3, error/4
%% --------------------------------------------------------------------
error(Msg) -> console_logging(error, Msg, none, unknown, unknown).
error(Msg, Vars) -> console_logging(error, Msg, Vars, unknown, unknown).
error(Msg, File, Line) -> console_logging(error, Msg, none, File, Line).
error(Msg, Vars, File, Line) -> console_logging(error, Msg, Vars, File, Line).

%% ====================================================================
%% Local Functions
%% ====================================================================

cat2lbl(log) -> "LOG";
cat2lbl(debug) -> "DEBUG";
cat2lbl(info) -> "INFO";
cat2lbl(warn) -> "WARN";
cat2lbl(error) -> "ERROR".

cat2pri(log) -> 5;
cat2pri(debug) -> 4;
cat2pri(info) -> 3;
cat2pri(warn) -> 2;
cat2pri(error) -> 1.

console_logging(Category, Msg, Vars, File, Line) ->
    Header = log_header(Category),
    Body = log_body(Msg, Vars),
    Footer = log_footer(File, Line),
    log_line(io_lib:format("~s~s~s", [Header, Body, Footer])).

log_header(Category) ->
    Now = timelib:now(),
    io_lib:format("~-5s ~-10w ~-24s ", [cat2lbl(Category), self(),
                                       timelib:to_string(Now)]).

log_body(Msg, none) -> Msg;
log_body(Msg, Vars) -> io_lib:format(Msg, Vars).

log_footer(unknown, unknown) -> "";
log_footer(File, Line) -> io_lib:format(" (~s:~w)", [File, Line]).

log_line(Line) ->
    io:format("~s~n", [Line]).
