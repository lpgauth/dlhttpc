%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @private
%%% @author Oscar Hellström <oscar@hellstrom.st>
%%% @doc
%%% This module implements various library functions used in dlhttpc.
-module(dlhttpc_lib).

-export([
        parse_url/1,
        format_request/7,
        header_value/2,
        header_value/3
    ]).
-export([to_binary/1]).

-export([format_hdrs/1, dec/1]).

-export([lookup/2]).

-include("dlhttpc_types.hrl").

%% @spec header_value(Header, Headers) -> undefined | term()
%% Header = string()
%% Headers = [{string(), term()}]
%% Value = term()
%% @doc
%% Returns the value associated with the `Header' in `Headers'.
%% `Header' must be a lowercase string, since every header is mangled to
%% check the match.
%% @end
-spec header_value(string(), [{string(), Value}]) -> undefined | Value.
header_value(Hdr, Hdrs) ->
    header_value(Hdr, Hdrs, undefined).

%% @spec header_value(Header, Headers, Default) -> Default | term()
%% Header = string()
%% Headers = [{string(), term()}]
%% Value = term()
%% Default = term()
%% @doc
%% Returns the value associated with the `Header' in `Headers'.
%% `Header' must be a lowercase string, since every header is mangled to
%% check the match.  If no match is found, `Default' is returned.
%% @end
-spec header_value(string(), [{string(), Value}], Default) ->
    Default | Value.
header_value(Hdr, [{Hdr, Value} | _], _) ->
    Value;
header_value(Hdr, [{ThisHdr, Value}| Hdrs], Default) ->
    case binary:match(ThisHdr, Hdr) of
        nomatch -> header_value(Hdr, Hdrs, Default);
        _Match  -> Value
    end;
header_value(_, [], Default) ->
    Default.

%% @spec (Item) -> OtherItem
%%   Item = atom() | list()
%%   OtherItem = list()
%% @doc
%% Will make any item, being an atom or a list, in to a list. If it is a
%% list, it is simple returned.
%% @end
-spec to_binary(atom() | binary()) -> binary().
to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, latin1);
to_binary(List) when is_list(List) ->
    list_to_binary(List);
to_binary(Binary) when is_binary(Binary) ->
    Binary.

%% @spec (URL) -> {Host, Port, Path, Ssl}
%%   URL = string()
%%   Host = string()
%%   Port = integer()
%%   Path = string()
%%   Ssl = boolean()
%% @doc
-spec parse_url(string()) -> {string(), integer(), string(), boolean()}.
parse_url(URL) ->
    % XXX This should be possible to do with the re module?
    {Scheme, HostPortPath} = split_scheme(URL),
    {Host, PortPath} = split_host(HostPortPath),
    {Port, Path} = split_port(Scheme, PortPath),
    {Host, Port, Path, Scheme =:= https}.

split_scheme(<<"http://", HostPortPath/binary>>) ->
    {http, HostPortPath};
split_scheme(<<"https://", HostPortPath/binary>>) ->
    {https, HostPortPath}.

split_host(HostPortPath) ->
    PortPath2 =
        case binary:split(HostPortPath, <<":">>, [trim]) of
            [HostPath] ->
                case binary:split(HostPath, <<"/">>, [trim]) of
                    [Host] ->
                        <<"/">>;
                    [Host, Path] ->
                        <<"/", Path/binary>>
                end;
            [Host, PortPath] ->
                PortPath
        end,
    {Host, PortPath2}.

split_port(http, <<"/", _Path/binary>> = Path) ->
    {80, Path};
split_port(https, <<"/", _Path/binary>> = Path) ->
    {443, Path};
split_port(_Scheme, PortPath) ->
    [Port | Path] = binary:split(PortPath, <<"/">>, [trim]),
    Path2 = case Path of
        [] -> <<"/">>;
        [Path3] -> <<"/", Path3/binary>>
    end,
    {binary_to_integer(Port), Path2}.

%% @spec (Path, Method, Headers, Host, Port, Body, PartialUpload) -> Request
%% Path = iolist()
%% Method = atom() | string()
%% Headers = [{atom() | string(), string()}]
%% Host = string()
%% Port = integer()
%% Body = iolist()
%% PartialUpload = true | false
-spec format_request(iolist(), atom() | string(), headers(), string(),
    integer(), iolist(), true | false ) -> {true | false, iolist()}.
format_request(Path, Method, Hdrs, Host, Port, Body, PartialUpload) ->
    AllHdrs = add_mandatory_hdrs(Method, Hdrs, Host, Port, Body, PartialUpload),
    IsChunked = is_chunked(AllHdrs),
    Request = [
        <<Method/binary, " ", Path/binary, " HTTP/1.1\r\n">>,
        format_hdrs(AllHdrs),
        format_body(Body, IsChunked)
    ],
    {IsChunked, Request}.

-spec format_hdrs(headers()) -> iolist().
format_hdrs(Headers) ->
    format_hdrs(Headers, []).

format_hdrs([{Hdr, Value} | T], Acc) ->
    NewAcc = [
        <<Hdr/binary, ": ", (maybe_integer_to_binary(Value))/binary, "\r\n">> | Acc
    ],
    format_hdrs(T, NewAcc);
format_hdrs([], Acc) ->
    [Acc, <<"\r\n">>].

format_body(Body, false) ->
    Body;
format_body(Body, true) ->
    case iolist_size(Body) of
        0 ->
            <<>>;
        Size ->
            [
                erlang:integer_to_list(Size, 16), <<"\r\n">>,
                Body, <<"\r\n">>
            ]
    end.

add_mandatory_hdrs(Method, Hdrs, Host, Port, Body, PartialUpload) ->
    ContentHdrs = add_content_headers(Method, Hdrs, Body, PartialUpload),
    add_host(ContentHdrs, Host, Port).

add_content_headers(<<"POST">>, Hdrs, Body, PartialUpload) ->
    add_content_headers(Hdrs, Body, PartialUpload);
add_content_headers(<<"PUT">>, Hdrs, Body, PartialUpload) ->
    add_content_headers(Hdrs, Body, PartialUpload);
add_content_headers(_, Hdrs, _, _PartialUpload) ->
    Hdrs.

add_content_headers(Hdrs, Body, false) ->
    case header_value(<<"Content-Length">>, Hdrs) of
        undefined ->
            ContentLength = iolist_size(Body),
            [{<<"Content-Length">>, ContentLength} | Hdrs];
        _ -> % We have a content length
            Hdrs
    end;
add_content_headers(Hdrs, _Body, true) ->
    case {header_value(<<"Content-Length">>, Hdrs),
         header_value(<<"Transfer-Encoding">>, Hdrs)} of
        {undefined, undefined} ->
            [{<<"Transfer-Encoding">>, <<"chunked">>} | Hdrs];
        {undefined, TransferEncoding} ->
            case TransferEncoding of
                <<"Chunked">> -> Hdrs;
                <<"chunked">> -> Hdrs;
                _ -> erlang:error({error, unsupported_transfer_encoding})
            end;
        {_Length, undefined} ->
            Hdrs;
        {_Length, _TransferEncoding} -> %% have both cont.length and chunked
            erlang:error({error, bad_header})
    end.

add_host(Hdrs, Host, Port) ->
    case header_value(<<"Host">>, Hdrs) of
        undefined ->
            [{<<"Host">>, host(Host, Port) } | Hdrs];
        _ -> % We have a host
            Hdrs
    end.

is_chunked(Hdrs) ->
    TransferEncoding = header_value(<<"Transfer-Encoding">>, Hdrs, "undefined"),
    case TransferEncoding of
        <<"Chunked">> -> true;
        <<"chunked">> -> true;
        _ -> false
    end.

-spec dec(timeout()) -> timeout().
dec(Num) when is_integer(Num) -> Num - 1;
dec(Else)                     -> Else.

host(Host, 80)   -> Host;
host(Host, Port) -> <<Host/binary, $:, (integer_to_binary(Port))/binary>>.

maybe_integer_to_binary(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
maybe_integer_to_binary(Binary) when is_binary(Binary) ->
    Binary.

lookup(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false -> undefined;
        {_, Value} -> Value
    end.
