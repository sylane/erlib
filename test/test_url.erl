%% ===========================================================================
%% @doc        Module url unit tests.
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

-module(test_url).

-author('Sebastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

-include("url.hrl").

%% --------------------------------------------------------------------
%% Imports
%% --------------------------------------------------------------------

-import(url, [new/0,
              format/1,
              parse/1,
              add_to_path/2,
              extend_path/2,
              lookup_query/2,
              lookup_query/3,
              fold_query/3,
              add_to_query/3,
              extend_query/2,
              format_location/1]).


%% ====================================================================
%% Unit Tests
%% ====================================================================

unqualified_url_path_format_test() ->
    Url1a = #url{},
    ?assertEqual("/", format(Url1a)),
    Url2a = #url{path = [abs, "foo"]},
    ?assertEqual("/foo", format(Url2a)),
    Url3a = add_to_path("", Url2a),
    ?assertEqual("/foo/", format(Url3a)),
    Url4a = add_to_path("bar", Url2a),
    ?assertEqual("/foo/bar", format(Url4a)),
    Url5a = add_to_path("", Url4a),
    ?assertEqual("/foo/bar/", format(Url5a)),
    Url6a = #url{path = [abs, "", "", ""]},
    ?assertEqual("///", format(Url6a)),
    Url7a = #url{path = [abs]},
    ?assertEqual("/", format(Url7a)),

    Url2r = #url{path = [rel, "foo"]},
    ?assertEqual("foo", format(Url2r)),
    Url3r = add_to_path("", Url2r),
    ?assertEqual("foo/", format(Url3r)),
    Url4r = add_to_path("bar", Url2r),
    ?assertEqual("foo/bar", format(Url4r)),
    Url5r = add_to_path("", Url4r),
    ?assertEqual("foo/bar/", format(Url5r)),
    Url6r = #url{path = [rel, "foo", "", "", ""]},
    ?assertEqual("foo///", format(Url6r)),
    Url7r = #url{path = [rel, ""]},
    ?assertEqual("", format(Url7r)),
    ok.


unqualified_url_query_format_test() ->
    Fun = fun(K, Vs, Acc) -> [{K, Vs} |Acc] end,
    Base = #url{path = [rel, "foo"]},
    Url1 = extend_query([{"k", "v"}], Base),
    ?assertEqual("foo?k=v", format(Url1)),
    ?assertEqual([{"k", ["v"]}], fold_query(Fun, [], Url1)),
    Url2 = extend_query([{"k", "v"}, {"k2", "v2"}], Base),
    ?assertEqual("foo?k=v&k2=v2", format(Url2)),
    ?assertEqual([{"k2", ["v2"]}, {"k", ["v"]}], fold_query(Fun, [], Url2)),
    Url3 = extend_query([{"k", "v"}, {"k2", "v2"}, {"k", "v3"}], Base),
    ?assertEqual("foo?k=v&k=v3&k2=v2", format(Url3)),
    ?assertEqual([{"k2", ["v2"]}, {"k", ["v", "v3"]}],
                 fold_query(Fun, [], Url3)),
    Url4 = extend_query([{"z", "a"}, {"a", "z"}, {"k", "k"}], Base),
    ?assertEqual("foo?a=z&k=k&z=a", format(Url4)),
    ?assertEqual([{"z", ["a"]}, {"k", ["k"]}, {"a", ["z"]}],
                 fold_query(Fun, [], Url4)),
    Url5 = extend_query([{"k", ""}, {"k", ""}, {"k", ""}], Base),
    ?assertEqual("foo?k=&k=&k=", format(Url5)),
    ?assertEqual([{"k", ["", "", ""]}], fold_query(Fun, [], Url5)),
    Url6 = add_to_query("k", "v", #url{path = [rel, ""]}),
    ?assertEqual("?k=v", format(Url6)),
    ok.

unqualified_url_fragment_format_test() ->
    Base = #url{path = [rel, "foo"]},
    Url1 = Base#url{frag = "top"},
    ?assertEqual("foo#top", format(Url1)),
    Url2 = Base#url{frag = ""},
    ?assertEqual("foo#", format(Url2)),
    ok.

unqualified_url_combined_format_test() ->
    Base = #url{path = [abs, "foo", "bar"]},
    Url1 = Base#url{frag = "top"},
    Url2 = add_to_query("key", "value", Url1),
    ?assertEqual("/foo/bar?key=value#top", format(Url2)),
    ok.

qualified_url_domain_format_test() ->
    Base = #url{scheme = http, domain = "domain"},
    Url1 = Base#url{domain = "do.main.lan"},
    ?assertEqual("http://do.main.lan/", format(Url1)),
    Url2 = Base#url{scheme = https, path = [abs, "foo"]},
    ?assertEqual("https://domain/foo", format(Url2)),
    Url3 = Base#url{scheme = ftp, user = "user"},
    ?assertEqual("ftp://user@domain/", format(Url3)),
    Url4 = Base#url{user = "user", pass = "pass"},
    ?assertEqual("http://user:pass@domain/", format(Url4)),
    Url5 = Base#url{pass = "pass"},
    ?assertEqual("http://domain/", format(Url5)),
    Url6 = Base#url{port = 8080},
    ?assertEqual("http://domain:8080/", format(Url6)),
    Url7 = Base#url{user = "user", port = 8080},
    ?assertEqual("http://user@domain:8080/", format(Url7)),
    Url8 = Base#url{user = "user", pass = "pass", port = 8080},
    ?assertEqual("http://user:pass@domain:8080/", format(Url8)),
    Url9 = Base#url{user = "user", pass = "", port = 8080},
    ?assertEqual("http://user:@domain:8080/", format(Url9)),
    Url10 = Base#url{user = "", pass = "pass", port = 8080},
    ?assertEqual("http://:pass@domain:8080/", format(Url10)),
    Url11 = Base#url{user = "", pass = "", port = 8080},
    ?assertEqual("http://:@domain:8080/", format(Url11)),
    Url12 = Base#url{user = "user", pass = ""},
    ?assertEqual("http://user:@domain/", format(Url12)),
    Url13 = Base#url{user = "", pass = "pass"},
    ?assertEqual("http://:pass@domain/", format(Url13)),
    Url14 = Base#url{user = "", pass = ""},
    ?assertEqual("http://:@domain/", format(Url14)),
    ok.

qualified_url_path_format_test() ->
    Url1a = #url{scheme = ftp, domain = "domain"},
    ?assertEqual("ftp://domain/", format(Url1a)),
    Url2a = Url1a#url{path = [abs, "foo"]},
    ?assertEqual("ftp://domain/foo", format(Url2a)),
    Url3a = add_to_path("", Url2a),
    ?assertEqual("ftp://domain/foo/", format(Url3a)),
    Url4a = add_to_path("bar", Url2a),
    ?assertEqual("ftp://domain/foo/bar", format(Url4a)),
    Url5a = add_to_path("", Url4a),
    ?assertEqual("ftp://domain/foo/bar/", format(Url5a)),
    Url6a = Url1a#url{path = [abs, "", "", ""]},
    ?assertEqual("ftp://domain///", format(Url6a)),
    ok.

qualified_url_query_format_test() ->
    Fun = fun(K, Vs, Acc) -> [{K, Vs} |Acc] end,
    Base = #url{scheme = http, domain = "domain", path = [abs, "foo"]},
    Url1 = extend_query([{"k", "v"}], Base),
    ?assertEqual("http://domain/foo?k=v", format(Url1)),
    ?assertEqual([{"k", ["v"]}], fold_query(Fun, [], Url1)),
    Url2 = extend_query([{"k", "v"}, {"k2", "v2"}], Base),
    ?assertEqual("http://domain/foo?k=v&k2=v2", format(Url2)),
    ?assertEqual([{"k2", ["v2"]}, {"k", ["v"]}], fold_query(Fun, [], Url2)),
    Url3 = extend_query([{"k", "v"}, {"k2", "v2"}, {"k", "v3"}], Base),
    ?assertEqual("http://domain/foo?k=v&k=v3&k2=v2", format(Url3)),
    ?assertEqual([{"k2", ["v2"]}, {"k", ["v", "v3"]}],
                 fold_query(Fun, [], Url3)),
    Url4 = extend_query([{"z", "a"}, {"a", "z"}, {"k", "k"}], Base),
    ?assertEqual("http://domain/foo?a=z&k=k&z=a", format(Url4)),
    ?assertEqual([{"z", ["a"]}, {"k", ["k"]}, {"a", ["z"]}],
                 fold_query(Fun, [], Url4)),
    Url5 = extend_query([{"k", ""}, {"k", ""}, {"k", ""}], Base),
    ?assertEqual("http://domain/foo?k=&k=&k=", format(Url5)),
    ?assertEqual([{"k", ["", "", ""]}], fold_query(Fun, [], Url5)),
    Url6 = extend_query([{"bar", "baz"}], Base),
    ?assertEqual("http://domain/foo?bar=baz", format(Url6)),
    ok.

qualified_url_fragment_format_test() ->
    Base = #url{scheme = http, domain = "domain", path = [abs, "foo"]},
    Url1 = Base#url{frag = "top"},
    ?assertEqual("http://domain/foo#top", format(Url1)),
    Url2 = Base#url{frag = ""},
    ?assertEqual("http://domain/foo#", format(Url2)),
    ok.

qualified_url_combined_format_test() ->
    Base = #url{scheme = http, domain = "domain", path = [abs, "foo", "bar"]},
    Url1 = Base#url{frag = "top"},
    Url2 = add_to_query("key", "value", Url1),
    ?assertEqual("http://domain/foo/bar?key=value#top", format(Url2)),
    ok.

url_encoding_format_test() ->
    Base = #url{scheme=http, domain="localhost", path = [abs, "file"]},
    Url1 = Base#url{user="#$ :/", pass="@&?)", path = [abs, ""]},
    ?assertEqual("http://%23$%20%3A%2F:%40%26%3F)@localhost/", format(Url1)),
    Url2 = Base#url{path = [abs, "~/sub/str", "with space"]},
    ?assertEqual("http://localhost/%7E%2Fsub%2Fstr/with%20space", format(Url2)),
    Url3 = add_to_query("=?& $+!*", "'() ,#;/", Base),
    ?assertEqual("http://localhost/file?%3D%3F%26+%24%2B%21%2A"
                 "=%27%28%29+%2C%23%3B%2F", format(Url3)),
    Url4 = Base#url{frag=";/?a"},
    ?assertEqual("http://localhost/file#%3B%2F%3Fa", format(Url4)),
    % Checks what do not have to be escaped
    Url5 = Base#url{user="aA$-_.+!*'(),zZ", pass="aA$-_.+!*'(),zZ",
                    path = [abs, "aA$-_.+!*'(),zZ",
                            "abcdefghijklmnopqrstuvwxyz",
                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"],
                    frag="aA$-_.+!*'(),zZ"},
    Url6 = add_to_query("aA.-~_zZ", "aA.-~_zZ", Url5),
    ?assertEqual("http://aA$-_.+!*'(),zZ:aA$-_.+!*'(),zZ@localhost"
                 "/aA$-_.+!*'(),zZ/abcdefghijklmnopqrstuvwxyz"
                 "/ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                 "?aA.-~_zZ=aA.-~_zZ"
                 "#aA$-_.+!*'(),zZ", format(Url6)),
    ok.

unqualified_url_path_parse_test() ->
    Url1 = #url{path = [rel, ""]},
    ?assertEqual(Url1, parse("")),
    Url2 = #url{path = [rel, "foo"]},
    ?assertEqual(Url2, parse("foo")),
    Url3 = #url{path = [rel, "foo", ""]},
    ?assertEqual(Url3, parse("foo/")),
    Url4 = #url{path = [rel, "foo", "bar"]},
    ?assertEqual(Url4, parse("foo/bar")),
    Url5 = #url{path = [rel, "foo", "bar", ""]},
    ?assertEqual(Url5, parse("foo/bar/")),

    Url6 = #url{path = [abs, ""]},
    ?assertEqual(Url6, parse("/")),
    Url7 = #url{path = [abs, "foo"]},
    ?assertEqual(Url7, parse("/foo")),
    Url8 = #url{path = [abs, "foo", ""]},
    ?assertEqual(Url8, parse("/foo/")),
    Url9 = #url{path = [abs, "foo", "bar"]},
    ?assertEqual(Url9, parse("/foo/bar")),
    Url10 = #url{path = [abs, "foo", "bar", ""]},
    ?assertEqual(Url10, parse("/foo/bar/")),
    ok.

unqualified_url_query_parse_test() ->
    Base = #url{path = [rel, "foo"]},
    Url1 = extend_query([{"k", "v"}], Base),
    ?assertEqual(Url1, parse("foo?k=v")),
    Url2 = extend_query([{"k", "v"}, {"k2", "v2"}], Base),
    ?assertEqual(Url2, parse("foo?k=v&k2=v2")),
    Url3 = extend_query([{"k", "v"}, {"k2", "v2"}, {"k", "v3"}], Base),
    ?assertEqual(Url3, parse("foo?k=v&k=v3&k2=v2")),
    Url4 = extend_query([{"z", "a"}, {"a", "z"}, {"k", "k"}], Base),
    ?assertEqual(Url4, parse("foo?a=z&k=k&z=a")),
    Url5 = extend_query([{"k", ""}, {"k", ""}, {"k", ""}], Base),
    ?assertEqual(Url5, parse("foo?k=&k=&k=")),
    Url6 = add_to_query("k", "v", #url{path=[rel, ""]}),
    ?assertEqual(Url6, parse("?k=v")),
    Url7 = extend_query([{"bar", "baz"}], Base),
    ?assertEqual(Url7, parse("foo?bar=baz")),
    ok.

unqualified_url_fragment_parse_test() ->
    Base = #url{path = [rel, "foo"]},
    Url1 = Base#url{frag = "top"},
    ?assertEqual(Url1, parse("foo#top")),
    Url2 = Base#url{frag = ""},
    ?assertEqual(Url2, parse("foo#")),
    ok.

qualified_url_domain_parse_test() ->
    Base = #url{scheme = http, domain = "domain"},
    Url1 = Base#url{domain = "do.main.lan"},
    ?assertEqual(Url1, parse("http://do.main.lan/")),
    Url2 = Base#url{scheme = https, path = [abs, "foo"]},
    ?assertEqual(Url2, parse("https://domain/foo")),
    Url3 = Base#url{scheme = ftp, user = "user"},
    ?assertEqual(Url3, parse("ftp://user@domain/")),
    Url4 = Base#url{user = "user", pass = "pass"},
    ?assertEqual(Url4, parse("http://user:pass@domain/")),
    Url6 = Base#url{port = 8080},
    ?assertEqual(Url6, parse("http://domain:8080/")),
    Url7 = Base#url{user = "user", port = 8080},
    ?assertEqual(Url7, parse("http://user@domain:8080/")),
    Url8 = Base#url{user = "user", pass = "pass", port = 8080},
    ?assertEqual(Url8, parse("http://user:pass@domain:8080/")),
    Url9 = Base#url{user = "user", pass = "", port = 8080},
    ?assertEqual(Url9, parse("http://user:@domain:8080/")),
    Url10 = Base#url{user = "", pass = "pass", port = 8080},
    ?assertEqual(Url10, parse("http://:pass@domain:8080/")),
    Url11 = Base#url{user = "", pass = "", port = 8080},
    ?assertEqual(Url11, parse("http://:@domain:8080/")),
    Url12 = Base#url{user = "user", pass = ""},
    ?assertEqual(Url12, parse("http://user:@domain/")),
    Url13 = Base#url{user = "", pass = "pass"},
    ?assertEqual(Url13, parse("http://:pass@domain/")),
    Url14 = Base#url{user = "", pass = ""},
    ?assertEqual(Url14, parse("http://:@domain/")),
    ok.

qualified_url_path_parse_test() ->
    Url1a = #url{scheme = ftp, domain = "domain"},
    ?assertEqual(Url1a, parse("ftp://domain/")),
    Url2a = Url1a#url{path = [abs, "foo"]},
    ?assertEqual(Url2a, parse("ftp://domain/foo")),
    Url3a = add_to_path("", Url2a),
    ?assertEqual(Url3a, parse("ftp://domain/foo/")),
    Url4a = add_to_path("bar", Url2a),
    ?assertEqual(Url4a, parse("ftp://domain/foo/bar")),
    Url5a = add_to_path("", Url4a),
    ?assertEqual(Url5a, parse("ftp://domain/foo/bar/")),
    Url6a = Url1a#url{path = [abs, "", "", ""]},
    ?assertEqual(Url6a, parse("ftp://domain///")),
    ok.

qualified_url_query_parse_test() ->
    Base = #url{scheme = http, domain = "domain", path = [abs, "foo"]},
    Url1 = extend_query([{"k", "v"}], Base),
    ?assertEqual(Url1, parse("http://domain/foo?k=v")),
    Url2 = extend_query([{"k", "v"}, {"k2", "v2"}], Base),
    ?assertEqual(Url2, parse("http://domain/foo?k=v&k2=v2")),
    Url3 = extend_query([{"k", "v"}, {"k2", "v2"}, {"k", "v3"}], Base),
    ?assertEqual(Url3, parse("http://domain/foo?k=v&k=v3&k2=v2")),
    Url4 = extend_query([{"z", "a"}, {"a", "z"}, {"k", "k"}], Base),
    ?assertEqual(Url4, parse("http://domain/foo?a=z&k=k&z=a")),
    Url5 = extend_query([{"k", ""}, {"k", ""}, {"k", ""}], Base),
    ?assertEqual(Url5, parse("http://domain/foo?k=&k=&k=")),
    Url6 = extend_query([{"bar", "baz"}], Base),
    ?assertEqual(Url6, parse("http://domain/foo?bar=baz")),
    ok.

url_encoding_parse_test() ->
    Base = #url{scheme=http, domain="localhost", path = [abs, "file"]},
    Url1 = Base#url{user="#$ :/", pass="@&?)", path = [abs, ""]},
    ?assertEqual(Url1, parse("http://%23$%20%3A%2F:%40%26%3F)@localhost/")),
    Url2 = Base#url{path = [abs, "~/sub/str", "with space"]},
    ?assertEqual(Url2, parse("http://localhost/%7E%2Fsub%2Fstr/with%20space")),
    Url3 = add_to_query("=?& $+!*", "'() ,#;/", Base),
    ?assertEqual(Url3, parse("http://localhost/file"
                             "?%3D%3F%26+%24%2B%21%2A"
                             "=%27%28%29+%2C%23%3B%2F")),
    Url4 = Base#url{frag=";/?a"},
    ?assertEqual(Url4, parse("http://localhost/file#%3B%2F%3Fa")),
    % Checks what do not have to be escaped
    Url5 = Base#url{user="aA$-_.+!*'(),zZ", pass="aA$-_.+!*'(),zZ",
                    path = [abs, "aA$-_.+!*'(),zZ",
                            "abcdefghijklmnopqrstuvwxyz",
                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ"],
                    frag="aA$-_.+!*'(),zZ"},
    Url6 = add_to_query("aA.-~_zZ", "aA.-~_zZ", Url5),
    ?assertEqual(Url6, parse("http://aA$-_.+!*'(),zZ:aA$-_.+!*'(),zZ@localhost"
                             "/aA$-_.+!*'(),zZ/abcdefghijklmnopqrstuvwxyz"
                             "/ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             "?aA.-~_zZ=aA.-~_zZ"
                             "#aA$-_.+!*'(),zZ")),
    ok.
