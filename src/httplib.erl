%% ===========================================================================
%% @doc        HTTP utility functions.
%% @since      May 07, 2010
%% @version    1.0
%% @copyright  2009, Sebastien Merle <s.merle@gmail.com>
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

-module(httplib).

-author('Sebastien Merle <s.merle@gmail.com>').


%% --------------------------------------------------------------------
%% Exports
%% --------------------------------------------------------------------

%% API exports
-export([parse_host/1,
         response_code/1,
         response_message/1,
         combined_header/1]).


%% ===================================================================
%% API Functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @spec parse_host(Host::list()) -> {string(), integer() | undefined}
%%
%% @doc Parses HTTP 'Host' header.

-spec parse_host(list()) -> {string(), integer() | undefined}.

parse_host(Host) ->
    case string:tokens(Host, ":") of
        [Name] -> {Name, undefined};
        [Name, Port] -> {Name, erlang:list_to_integer(Port)}
    end.


%% --------------------------------------------------------------------
%% @spec response_code(atom() | integer()) -> integer()
%%
%% @doc Gives HTTP response code for the specified response atom.

-spec response_code(atom() | integer()) -> integer().

response_code(ok)                              -> 200;
response_code(created)                         -> 201;
response_code(accepted)                        -> 202;
response_code(non_authoritative_information)   -> 203;
response_code(no_content)                      -> 204;
response_code(reset_content)                   -> 205;
response_code(partial_content)                 -> 206;
response_code(multi_status)                    -> 207;
response_code(multiple_choice)                 -> 300;
response_code(moved_permanently)               -> 301;
response_code(found)                           -> 302;
response_code(see_other)                       -> 303;
response_code(not_modified)                    -> 304;
response_code(use_proxy)                       -> 305;
response_code(temporary_redirect)              -> 307;
response_code(bad_request)                     -> 400;
response_code(unauthorized)                    -> 401;
response_code(payment_required)                -> 402;
response_code(forbidden)                       -> 403;
response_code(not_found)                       -> 404;
response_code(not_allowed)                     -> 405;
response_code(not_acceptable)                  -> 406;
response_code(proxy_auth_required)             -> 407;
response_code(request_timeout)                 -> 408;
response_code(conflict)                        -> 409;
response_code(gone)                            -> 410;
response_code(length_required)                 -> 411;
response_code(precondition_failed)             -> 412;
response_code(request_entity_too_large)        -> 413;
response_code(request_uri_too_long)            -> 414;
response_code(unsupported_media_type)          -> 415;
response_code(requested_range_not_satisfiable) -> 416;
response_code(expectation_failed)              -> 417;
response_code(internal_server_error)           -> 500;
response_code(not_implemented)                 -> 501;
response_code(bad_gateway)                     -> 502;
response_code(service_unavailable)             -> 503;
response_code(gateway_timeout)                 -> 504;
response_code(http_version_not_supported)      -> 505;
response_code(insufficient_storage_space)      -> 507;
response_code(not_extended)                    -> 510;
response_code(Code) when is_integer(Code)      -> Code.


%% --------------------------------------------------------------------
%% @spec response_message(atom() | string()) -> string()
%%
%% @doc Gives HTTP response message for the specified response atom.

-spec response_message(atom() | string()) -> string().

% 2XX:
response_message(ok)                     -> "OK";
response_message(created)                -> "Created";
response_message(accepted)               -> "Accepted";
response_message(non_authoritative_information)
                                         -> "Non-Authoritative Information";
response_message(no_content)             -> "No Content";
response_message(reset_content)          -> "Reset Content.";
response_message(partial_content)        -> "Partial Content";
response_message(multi_status)           -> "Multi-Status";
% 3XX:
response_message(multiple_choice)        -> "Multiple Choices";
response_message(moved_permanently)      -> "Moved Permanently";
response_message(found)                  -> "Found";
response_message(see_other)              -> "See Other";
response_message(not_modified)           -> "Not Modified";
response_message(use_proxy)              -> "Use Proxy";
response_message(temporary_redirect)     -> "Temporary Redirect";
% 4XX:
response_message(bad_request)            -> "Bad Request";
response_message(unauthorized)           -> "Unauthorized";
response_message(payment_required)       -> "Payment Required";
response_message(forbidden)              -> "Forbidden";
response_message(not_found)              -> "Not Found";
response_message(not_allowed)            -> "Method Not Allowed";
response_message(not_acceptable)         -> "Not Acceptable";
response_message(proxy_auth_required)    -> "Proxy Authentication Required";
response_message(request_timeout)        -> "Request Time-out";
response_message(conflict)               -> "Conflict";
response_message(gone)                   -> "Gone";
response_message(length_required)        -> "Length Required";
response_message(precondition_failed)    -> "Precondition Failed";
response_message(request_entity_too_large)
                                         -> "Request Entity Too Large";
response_message(request_uri_too_long)   -> "Request-URI Too Long";
response_message(unsupported_media_type) -> "Unsupported Media Type";
response_message(requested_range_not_satisfiable)
                                         -> "Requested Range not satisfiable";
response_message(expectation_failed)     -> "Expectation Failed";
% 5XX:
response_message(internal_server_error)  -> "Internal Server Error";
response_message(not_implemented)        -> "Not Implemented";
response_message(bad_gateway)            -> "Bad Gateway";
response_message(service_unavailable)    -> "Service Unavailable";
response_message(gateway_timeout)        -> "Gateway Time-out";
response_message(http_version_not_supported)
                                         -> "HTTP Version not supported";
response_message(insufficient_storage_space)
                                         -> "Insufficient Storage Space";
response_message(not_extended)           -> "Not Extended".


%% --------------------------------------------------------------------
%% @spec combined_header(atom()) -> boolean()
%%
%% @doc Returns is specified header is a combined header that could 
%% be specified multiple times. A list of all headers should be used instead
%% of just the last one.

-spec combined_header(atom()) -> boolean().

combined_header('Accept') -> true;
combined_header('Accept-Charset') -> true;
combined_header('Accept-Encoding') -> true;
combined_header('Accept-Language') -> true;
combined_header('Allow') -> true;
combined_header('Cache-Control') -> true;
combined_header('Connection') -> true;
combined_header('Content-Encoding') -> true;
combined_header('Content-Language') -> true;
combined_header('Expect') -> true;
combined_header('Pragma') -> true;
combined_header('Proxy-Authenticate') -> true;
combined_header('Transfer-Encoding') -> true;
combined_header('Upgrade') -> true;
combined_header('Via') -> true;
combined_header('Warning') -> true;
combined_header('Www-Authenticate') -> true;
combined_header(<<"Accept-Ranges">>) -> true;
combined_header(<<"Te">>) -> true;
combined_header(<<"Trailer">>) -> true;
combined_header(_) -> false.
