%% ===========================================================================
%% @doc        Module opts unit tests.
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

-module(test_opts).

-author('Sebastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% includes
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Imports
%% --------------------------------------------------------------------

-import(opts, [split/1]).

%% ===================================================================
%% Unit Tests
%% ====================================================================

-define(equal(V, E), ?_assertEqual(E, split(V))).
-define(error(V, E), ?_assertThrow({opts_error, E, _}, split(V))).

noquote_test_() ->
  [?equal("", []),
   ?equal("a", ["a"]),
   ?equal("aaaaa", ["aaaaa"]),
   ?equal("    aaa", ["aaa"]),
   ?equal("aaa  ", ["aaa"]),
   ?equal("  aaa   ", ["aaa"]),
   ?equal("a b c", ["a", "b", "c"]),
   ?equal("  a   b c  ", ["a", "b", "c"]),
   ?equal("a A a A", ["a", "A", "a", "A"]),
   ?equal("  aaaa  bb c   dddddddddd",
      ["aaaa", "bb", "c", "dddddddddd"])
   ].

noquote_escape_test_() ->
  [?error("\\", wrong_escape),
   ?equal("\\a", ["a"]),
   ?equal("\\a\\b\\c\\d", ["abcd"]),
   ?equal("\\ ", [" "]),
   ?equal("  aa\\a \\ \\ \\  \\bbb\\   \\ cc \\  dddd ",
      ["aaa", "   ", "bbb ", " cc", " ", "dddd"]),
   ?equal("\\\"", ["\""]),
   ?equal("  \\\"aaa\\\"   ", ["\"aaa\""]),
   ?equal("\\a\\\"\\c\\'", ["a\"c'"])
   ].

single_quote_test_() ->
  [?error("'", not_closed),
   ?error("'aaa bbb ccc", not_closed),
   ?error("aaa 'bbb' 'ccc", not_closed),
   ?equal(" aaa  '  bbb '   ccc", ["aaa", "  bbb ", "ccc"]),
   ?equal("' aaa 'bbb ccc", [" aaa bbb", "ccc"]),
   ?equal("aaa 'bbb 'ccc", ["aaa", "bbb ccc"])
   ].

single_quote_escape_test_() ->
  [
   ?equal("a 'b\\b\\\\b'  c", ["a", "b\\b\\\\b", "c"]),
   ?equal("a '\"b\\\"b\\\\\"b'  c", ["a", "\"b\\\"b\\\\\"b", "c"])
   ].

double_quote_test_() ->
  [?error("\"", not_closed),
   ?error("aaa \"bbb ccc", not_closed),
   ?error("aaa \"bbb\" \"ccc", not_closed),
   ?equal("aaa  \"  bbb \"   ccc", ["aaa", "  bbb ", "ccc"]),
   ?equal("aaa\" bbb\" ccc", ["aaa bbb", "ccc"]),
   ?equal("aaa \"bbb \"ccc", ["aaa", "bbb ccc"])
   ].

double_quote_escape_test_() ->
  [?error("\"\\\"\\\\\"\"", not_closed),
   ?equal("a \"b\\b\\\\b\" c", ["a", "b\\b\\b", "c"]),
   ?equal("\"'\\'\\\\'\"", ["'\\'\\'"]),
   ?equal("\"\\\"\\\\\"\\\\\\\"\\\\\\\\\"\"", ["\"\\\\\"\\\\"])
   ].

mixed_quote_test_() ->
  [?error("aaa \"  bbb '\" ccc'", not_closed),
   ?error("aaa '  bbb \"' ccc\"", not_closed),
   ?equal("'aaa' bbb \"ccc\"", ["aaa", "bbb", "ccc"]),
   ?equal("aaa \"  bbb\"' ccc'", ["aaa", "  bbb ccc"])
   ].
