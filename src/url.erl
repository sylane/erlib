%% ===========================================================================
%% @doc        URL parsing, manipulation and formating.
%%             Should comply with RFC 1738.
%%             TODO: Add url path parameters parsing / formating
%% @since      Dec 05, 2009
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

-module(url).
-author('Sebastien Merle <s.merle@gmail.com>').

%% --------------------------------------------------------------------
%% Includes
%% --------------------------------------------------------------------

-include("url.hrl").

-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([new/0,
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

%% ===================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @doc Creates a new URL with default values.
-spec new() -> url().
%% --------------------------------------------------------------------
new() -> #url{}.

%% --------------------------------------------------------------------
%% @doc Formats the specified URL as a string.
-spec format(Url::url()) -> string().
%% --------------------------------------------------------------------
format(Url) -> format_url(string, Url).

%% --------------------------------------------------------------------
%% @doc Creates a new URL by parsing a RFC1738 string representation.
-spec parse(UrlStr::string()) -> url().
%% --------------------------------------------------------------------
parse(UrlStr) -> parse_url(UrlStr).

%% --------------------------------------------------------------------
%% @doc Adds a path segment to the end of the specified URL's path.
-spec add_to_path(Segment::string(), Url::url()) -> url().
%% --------------------------------------------------------------------
add_to_path(Segment, #url{path = Path} = Url) ->
    Url#url{path = Path ++ [Segment]}.

%% --------------------------------------------------------------------
%% @doc Extends the path segments of the specified URL with the specified
%%      path segments.
-spec extend_path([Segment::string()], Url::url()) -> url().
%% --------------------------------------------------------------------
extend_path(Segments, #url{path = Path} = Url) ->
    Url#url{path = Path ++ Segments}.

%% --------------------------------------------------------------------
%% @doc Lookups specified URL's query string for a specified key.
%%      If the key is not found it raise a bad_arg error.
-spec lookup_query(Key::string(), Url::url()) -> [string()].
%% --------------------------------------------------------------------
lookup_query(_Key, #url{qry = undefined}) -> erlang:error(badarg);
lookup_query(Key, #url{qry = Qry}) -> orddict:fetch(Key, Qry).

%% --------------------------------------------------------------------
%% @doc Lookups specified URL's query string for a specified key.
%%      If the key is not found the specified default value is returned.
-spec lookup_query(Key::string(), Def, Url::url()) -> [string()] | Def.
%% --------------------------------------------------------------------
lookup_query(_Key, Default, #url{qry = undefined}) -> Default;
lookup_query(Key, Default, #url{qry = Qry}) ->
    try ordict:fetch(Key, Qry) catch
        error:badarg -> Default
    end.

%% --------------------------------------------------------------------
%% @doc Calls the specified function on successive keys and values pairs
%%      of the specified URL query string.
-type fold_query_fun() :: fun((Key::string(), Val::string(),
                               Acc::term()) -> term()).
-spec fold_query(Fun::fold_query_fun(), Acc0::term(), Url::url()) -> term().
%% --------------------------------------------------------------------
fold_query(_Fun, Acc0, #url{qry = undefined}) -> Acc0;
fold_query(Fun, Acc0, #url{qry = Qry}) -> orddict:fold(Fun, Acc0, Qry).

%% --------------------------------------------------------------------
%% @doc Adds a key/value paire to the specified URL's query string.
-spec add_to_query(Key::string(), Value::string(), Url::url()) -> url().
%% --------------------------------------------------------------------
add_to_query(Key, Val, #url{qry = undefined} = Url) ->
    Url#url{qry = orddict:append(Key, Val, orddict:new())};
add_to_query(Key, Val, #url{qry = Qry} = Url) ->
    Url#url{qry = orddict:append(Key, Val, Qry)}.

%% --------------------------------------------------------------------
%% @doc Extends the query string of the specified URL with the specified
%%      key/value paires.
-spec extend_query([{Key::string(), Value::string()}], Url::url()) -> url().
%% --------------------------------------------------------------------
extend_query([], Url) -> Url;
extend_query([{Key, Val} |Pairs], Url) ->
    extend_query(Pairs, add_to_query(Key, Val, Url)).

%% --------------------------------------------------------------------
%% @doc Formats the location part of the specified URL as a string.
%%      The location is the non-qualified URL (without scheme and domain).
-spec format_location(Url::url()) -> string().
%% --------------------------------------------------------------------
format_location(#url{path = P, qry = Q}) ->
    format_url(string, #url{path = P, qry = Q}).

%% ====================================================================
%% Local Functions
%% ====================================================================

rfc_encode(Str) -> encode(rfc, Str, []).

url_encode(Str) -> encode(url, Str, []).

encode(_K, [], Acc) ->
    lists:flatten(lists:reverse(Acc));
encode(K, [C |T], Acc) ->
    encode(K, is_safe(K, C), C, T, Acc).

encode(K, true, C, T, Acc) ->
    encode(K, T, [C |Acc]);
encode(url, false, $ , T, Acc) ->
    encode(url, T, [$+ |Acc]);
encode(K, false, C, T, Acc) ->
    encode(K, T, [erlang:integer_to_list(C, 16), $% |Acc]).

is_safe(rfc, C) ->
    % Source: http://www.ietf.org/rfc/rfc1738.txt
    if C =:= $! -> true;
       C =:= $$ -> true;
       C < $' -> false;
       C =< $. -> true;
       C < $0 -> false;
       C =< $9 -> true;
       C < $A -> false;
       C =< $Z -> true;
       C =:= $_ -> true;
       C < $a -> false;
       C =< $z -> true;
       true -> false
    end;
is_safe(url, C) ->
    % Source: http://en.wikipedia.org/wiki/Query_string
    if C < $- -> false;
       C =< $. -> true;
       C < $0 -> false;
       C =< $9 -> true;
       C < $A -> false;
       C =< $Z -> true;
       C =:= $_ -> true;
       C < $a -> false;
       C =< $z -> true;
       C =:= $~ -> true;
       true -> false
    end.

rfc_decode(Str) -> decode(rfc, Str, []).

url_decode(Str) -> decode(url, Str, []).

decode(_K, [], Acc) ->
    lists:reverse(Acc);
decode(K, [$%, Hi, Lo |T], Acc) ->
    decode(K, T, [erlang:list_to_integer([Hi, Lo], 16) |Acc]);
decode(url, [$+ |T], Acc) ->
    decode(url, T, [$  |Acc]);
decode(K, [C |T], Acc) ->
    decode(K, T, [C |Acc]).

format_url(F, #url{domain = undefined, path = [abs |_]} = Url) ->
    format_absolute(F, Url, []);
format_url(F, #url{domain = undefined, path = [rel |_]} = Url) ->
    format_relative(F, Url, []);
format_url(F, #url{path = [abs |_]} = Url) ->
    format_scheme(F, Url, []).

format_scheme(F, #url{scheme = Scheme, path = [abs |_]} = Url, Acc) ->
    format_auth(F, Url, ["://", erlang:atom_to_list(Scheme) |Acc]).

format_auth(F, #url{user = undefined} = Url, Acc) ->
    format_domain(F, Url, Acc);
format_auth(F, #url{user = Username, pass = undefined} = Url, Acc) ->
    format_domain(F, Url, [$@, rfc_encode(Username) |Acc]);
format_auth(F, #url{user = Username, pass = Password} = Url, Acc) ->
    format_domain(F, Url, [$@, rfc_encode(Password), $:, rfc_encode(Username) |Acc]).

format_domain(F, #url{domain = undefined, port = undefined} = Url, Acc) ->
    format_absolute(F, Url, ["localhost" |Acc]);
format_domain(F, #url{domain = Domain, port = undefined} = Url, Acc) ->
    format_absolute(F, Url, [Domain |Acc]);
format_domain(F, #url{domain = Domain, port = Port} = Url, Acc) ->
    format_absolute(F, Url, [erlang:integer_to_list(Port),
                             $:, Domain |Acc]).

format_absolute(F, #url{path = [abs |Parts]} = Url, Acc) ->
    Encoded = string:join([rfc_encode(P) || P <- Parts], "/"),
    format_query(F, Url, [Encoded, $/ |Acc]).

format_relative(F, #url{path = [rel |Parts]} = Url, Acc) ->
    Encoded = string:join([rfc_encode(P) || P <- Parts], "/"),
    format_query(F, Url, [Encoded |Acc]).

format_query(F, #url{qry = undefined} = Url, Acc) ->
    format_frag(F, Url, Acc);
format_query(F, #url{qry = Query} = Url, Acc) ->
    case orddict:fold(fun format_query_item/3, [$? |Acc], Query) of
        [$? |NewAcc] ->
            % No query string, removing the question mark
            format_frag(F, Url, NewAcc);
        [$& |NewAcc] ->
            % Removing the last delimiter
            format_frag(F, Url, NewAcc)
    end.

format_query_item(_Key, [], Acc) -> Acc;
format_query_item(Key, [Val |T], Acc) ->
    format_query_item(Key, T, [$&, url_encode(Val), $=, url_encode(Key) |Acc]).

format_frag(F, #url{frag = undefined}, Acc) ->
    format_output(F, Acc);
format_frag(F, #url{frag = Frag}, Acc) ->
    format_output(F, [rfc_encode(Frag), $# |Acc]).

format_output(string, Acc) ->
    lists:flatten(lists:reverse(Acc)).

parse_url(Str) -> parse_scheme(Str, Str, #url{}, []).

parse_scheme(Full, [], Url, _Acc) ->
    % Just a relative name, rollback already parsed characters
    parse_path(Full, Full, Url, [], []);
parse_scheme(Full, [$:, $/, $/ |T], Url, Acc) ->
    % Found the scheme
    parse_user(Full, T, set_scheme(Url, Acc), []);
parse_scheme(Full, [C |T], Url, Acc)
  when C >= $a andalso C =< $z; C >= $A andalso C =< $Z ->
    % Scheme are case insensitive and only alfa
    parse_scheme(Full, T, Url, [C |Acc]);
parse_scheme(Full, _P, Url, _Acc) ->
    % It's not a scheme, must be a just a path
    parse_path(Full, Full, Url, [], []).

set_scheme(Url, Acc) ->
    % Warning: may leak for lots of different scheme
    Scheme = erlang:list_to_atom(string:to_lower(lists:reverse(Acc))),
    Url#url{scheme = Scheme}.

parse_user(Full, [], _Url, []) ->
    % A scheme without the rest is invalid
    throw({bad_url, Full});
parse_user(_Full, [], Url, Acc) ->
    % It was a domain, and there is nothing more
    set_domain(Url, Acc);
parse_user(Full, [$: |T], Url, Acc) ->
    % User or domain with port
    parse_pass(Full, T, set_user(Url, Acc), []);
parse_user(Full, [$@ |T], Url, Acc) ->
    % There is no password
    parse_domain(Full, T, set_user(Url, Acc), []);
parse_user(Full, [$/ |_] = P, Url, Acc) ->
    % There is no user, password or port, only a domain
    parse_path(Full, P, set_domain(Url, Acc), [], []);
parse_user(Full, [C |T], Url, Acc) ->
    parse_user(Full, T, Url, [C |Acc]).

set_user(Url, Acc) ->
    % Decoding should be safe even if it results to be a domain
    User = rfc_decode(lists:reverse(Acc)),
    Url#url{user = User}.

parse_pass(Full, [], _Url, []) ->
    % No domain or no port
    throw({bad_url, Full});
parse_pass(_Full, [], Url, Acc) ->
    % It was a port not a pass, and there is nothing more
    Domain = string:to_lower(Url#url.user),
    set_port(Url#url{user = undefined, domain = Domain}, Acc);
parse_pass(Full, [$@ |T], Url, Acc) ->
    % Pass finished, now the domain
    parse_domain(Full, T, set_pass(Url, Acc), []);
parse_pass(Full, [$/ |T], Url, Acc) ->
    % It was a port not password, so now it's a path
    Domain = string:to_lower(Url#url.user),
    NewUrl = set_port(Url#url{user = undefined, domain = Domain}, Acc),
    parse_path(Full, T, NewUrl, [], []);
parse_pass(Full, [C |T], Url, Acc) ->
    parse_pass(Full, T, Url, [C |Acc]).

set_pass(Url, Acc) ->
    Pass = rfc_decode(lists:reverse(Acc)),
    Url#url{pass = Pass}.

parse_domain(Full, [], _Url, []) ->
    % Should have a domain if it has a scheme
    throw({bad_url, Full});
parse_domain(_Full, [], Url, Acc) ->
    % Just have a domain without path
    set_domain(Url, Acc);
parse_domain(Full, [$/ |_] = P, Url, Acc) ->
    % There is no port, now it is a path
    parse_path(Full, P, set_domain(Url, Acc), [], []);
parse_domain(Full, [$: |T], Url, Acc) ->
    % Now the port
    parse_port(Full, T, set_domain(Url, Acc), []);
parse_domain(Full, [C |T], Url, Acc) ->
    parse_domain(Full, T, Url, [C |Acc]).

set_domain(Url, Acc) ->
    % Domains are case insensitive
    Domain = string:to_lower(lists:reverse(Acc)),
    Url#url{domain = Domain}.

parse_port(Full, [], _Url, []) ->
    % Incomplete port specification
    throw({bad_url, Full});
parse_port(_Full, [], Url, Acc) ->
    % Ther is no path specified
    set_port(Url, Acc);
parse_port(Full, [$/ |_] = P, Url, Acc) ->
    % Now the path
    parse_path(Full, P, set_port(Url, Acc), [], []);
parse_port(Full, [C |T], Url, Acc) ->
    parse_port(Full, T, Url, [C |Acc]).

set_port(Url, Acc) ->
    Port = erlang:list_to_integer(lists:reverse(Acc)),
    Url#url{port = Port}.

parse_path(_Full, [], #url{scheme = undefined} = Url, [], []) ->
    % No path no scheme
    Url#url{path = [rel, ""]};
parse_path(_Full, [], Url, [], []) ->
    % No path with scheme, it's the root
    Url#url{path = [abs, ""]};
parse_path(_Full, [], Url, Parts, Acc) ->
    % Finished without query, or fragment
    set_path(Url, Parts, Acc);
parse_path(Full, [$/ |T], Url, [], []) ->
    % Absolute path
    parse_path(Full, T, Url, [abs], []);
parse_path(Full, [$/ |T], Url, Parts, Acc) ->
    % Splitting the path
    parse_path(Full, T, Url, [lists:reverse(Acc) |Parts], []);
parse_path(Full, [$? |T], Url, [], []) ->
    % Query string without path, assuming it's a relative path
    parse_query(Full, T, set_path(Url, [rel], ""));
parse_path(Full, [$? |T], Url, Parts, Acc) ->
    % Now comes a query string
    parse_query(Full, T, set_path(Url, Parts, Acc));
parse_path(Full, [$# |T], Url, Parts, Acc) ->
    % Now comes a fragment
    parse_frag(Full, T, set_path(Url, Parts, Acc), []);
parse_path(Full, [C |T], Url, [], []) ->
    % First char is not /, asuming it's a relative path
    parse_path(Full, T, Url, [rel], [C]);
parse_path(Full, [C |T], Url, Parts, Acc) ->
    parse_path(Full, T, Url, Parts, [C |Acc]).

set_path(Url, Parts, Acc) ->
     [Kind |Ordered] = lists:reverse([lists:reverse(Acc) |Parts]),
     Url#url{path = [Kind |[rfc_decode(P) || P <- Ordered]]}.

parse_query(_Full, [], Url) -> Url;
parse_query(Full, Str, Url) -> parse_query_key(Full, Str, Url, []).

parse_query_key(Full, [$= |T], Url, Acc) ->
    % Now comes the value
    parse_query_value(Full, T, Url, Acc, []);
parse_query_key(Full, [$& |T], Url, _Acc) ->
    % Key without value, just drop the key
    parse_query_key(Full, T, Url, []);
parse_query_key(Full, [$# |T], Url, _Acc) ->
    % Key without value, just drop the key
    parse_frag(Full, T, Url, []);
parse_query_key(Full, [C |T], Url, Acc) ->
    parse_query_key(Full, T, Url, [C |Acc]).

parse_query_value(_Full, [], Url, Key, Acc) ->
    set_query_item(Url, Key, Acc);
parse_query_value(Full, [$# |T], Url, Key, Acc) ->
    parse_frag(Full, T, set_query_item(Url, Key, Acc), []);
parse_query_value(Full, [$& |T], Url, Key, Acc) ->
    parse_query_key(Full, T, set_query_item(Url, Key, Acc), []);
parse_query_value(Full, [C |T], Url, Key, Acc) ->
    parse_query_value(Full, T, Url, Key, [C |Acc]).

set_query_item(Url, Key, Value) ->
   add_to_query(url_decode(lists:reverse(Key)),
                url_decode(lists:reverse(Value)), Url).

parse_frag(_Full, [], Url, Acc) ->
    set_frag(Url, Acc);
parse_frag(Full, [C |T], Url, Acc) ->
    parse_frag(Full, T, Url, [C |Acc]).

set_frag(Url, Acc) ->
    Url#url{frag = rfc_decode(lists:reverse(Acc))}.

%% ====================================================================
%% Tests
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
    Url7r = #url{path=[rel, ""]},
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
    Url6 = add_to_query("k", "v", #url{path=[rel, ""]}),
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
