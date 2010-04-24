%% ===========================================================================
%% @doc        Command line splitter supporting quotes and escapes.
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

-module(opts).

-author('Sebastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([split/1]).


%% ===================================================================
%% API Functions
%% ====================================================================

split(Str) -> split(Str, [], []).


%% ===================================================================
%% Internal Functions
%% ====================================================================

split([], R, []) -> lists:reverse(R);
split([], R, V) -> lists:reverse([lists:reverse(V) | R]);
split([$  | T] , R, []) -> split(T, R, []);
split([$  | T] , R, V) -> split(T, [lists:reverse(V) | R], []);
split([$" | T] , R, V) -> split_dq(T, R, V);
split([$' | T] , R, V) -> split_sq(T, R, V);
split([$\\ | T] , R, V) -> split_esc(T, R, V);
split([C | T] , R, V) -> split(T, R, [C | V]).

% Escaping always preserves the literal value
split_esc([], _R, _V) ->
  split_error(wrong_escape, "Escape character with nothing to escape");
split_esc([C | T], R, V) -> split(T, R, [C | V]).

% Inside single quotes
split_sq([] , _A, _V) ->
  split_error(not_closed, "Expecting single quote, group not closed");
split_sq([$' | T] , R, V) -> split(T, R, V);
split_sq([C | T] , R, V) -> split_sq(T, R, [C | V]).

% Inside double quotes
split_dq([] , _A, _V) ->
  split_error(not_closed, "Expecting double quote, group not closed");
split_dq([$\\ | T] , R, V) -> split_dq_esc(T, R, V);
split_dq([$" | T] , R, V) -> split(T, R, V);
split_dq([C | T] , R, V) -> split_dq(T, R, [C | V]).

% Escaping preserves the literal value of " and \, otherwise \ is keeped
split_dq_esc([C | T], R, V)
  when C == $\\; C == $" -> split_dq(T, R, [C | V]);
split_dq_esc(T, R, V) -> split_dq(T, R, [$\\ | V]).

split_error(E, M) -> throw({opts_error, E, M}).
