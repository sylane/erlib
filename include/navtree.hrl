%% ===========================================================================
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

-ifndef(IS_NAVTREE_INCLUDED).
-define(IS_NAVTREE_INCLUDED, true).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

% Name of the tree record
-define(NAVTREE_RECNAME, navtree).

% Macros to use in pattern matching
-define(NAVTREE_MATCH_ROOT(), #?NAVTREE_RECNAME{ids = []}).
-define(NAVTREE_MATCH_LEAF(), #?NAVTREE_RECNAME{children = []}).
-define(NAVTREE_MATCH_CURRENT(N), #?NAVTREE_RECNAME{node = N}).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

% Made public but should not be used directly, only with the given macros
-record(?NAVTREE_RECNAME, {id, node, children, ids, stack}).

%% --------------------------------------------------------------------
%% Types
%% --------------------------------------------------------------------

-type navtree() :: #?NAVTREE_RECNAME{}.
-type navtree_id() :: term().
-type navtree_path() :: [navtree_id()].

-endif.
