%% ===========================================================================
%% @doc        TODO: Add module optlib documentation.
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

-module(optlib).
-author('Sébastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([contains/2,
         set_defaults/2]).

%% ====================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% TODO: Add description of function contains/2
%% --------------------------------------------------------------------
contains([], _Opts) -> true;
contains([Opt |Rest], Opts) ->
    case contains(Opt, Opts) of
        true -> contains(Rest, Opts);
        false -> false
    end;
contains({Name, _DefVal}, Opts) ->
    case lists:keyfind(Name, 1, Opts) of
        false -> false;
        _CurrVal -> true
    end;
contains(Name, Opts) when is_atom(Name) ->
    lists:member(Name, Opts).

%% --------------------------------------------------------------------
%% TODO: Add description of function set_defaults/2
%% --------------------------------------------------------------------
set_defaults([], Opts) -> Opts;
set_defaults([Opt |Rest], Opts) ->
    set_defaults(Rest, set_defaults(Opt, Opts));
set_defaults({_Name, _Value} = Opt, Opts) ->
    set_default(Opt, Opts);
set_defaults(Opt, Opts) when is_atom(Opt) ->
    set_default(Opt, Opts).

%% ====================================================================
%% Local Functions
%% ====================================================================

set_default(Opt, Opts) ->
    case contains(Opt, Opts) of
        true -> Opts;
        false -> Opts ++ [Opt]
    end.
