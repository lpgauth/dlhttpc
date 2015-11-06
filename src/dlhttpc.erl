%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% Copyright (c) 2012, Frederic Trottier-Hebert
%%% Copyright (c) 2015, Louis-Philippe Gauthier
%%%
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

-module(dlhttpc).
-include("dlhttpc_types.hrl").

-export([
    kill_client/1,
    request/4,
    request/5,
    request/6,
    request/9,
    start/0,
    stop/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
kill_client(Pid) ->
    Monitor = erlang:monitor(process, Pid),
    unlink(Pid),
    exit(Pid, timeout),
    receive
        {response, _ReqId, Pid, R} ->
            erlang:demonitor(Monitor, [flush]),
            R;
        {'DOWN', _, process, Pid, timeout} ->
            {error, timeout};
        {'DOWN', _, process, Pid, Reason}  ->
            erlang:error(Reason)
    end.

-spec start() -> ok | {error, any()}.

start() ->
    application:start(dlhttpc).

-spec stop() -> ok | {error, any()}.

stop() ->
    application:stop(dlhttpc).

-spec request(binary(), binary(), headers(), pos_integer() |
    infinity) -> result().

request(URL, Method, Hdrs, Timeout) ->
    request(URL, Method, Hdrs, [], Timeout, []).

-spec request(binary(), binary(), headers(), iolist(),
        pos_integer() | infinity) -> result().

request(URL, Method, Hdrs, Body, Timeout) ->
    request(URL, Method, Hdrs, Body, Timeout, []).

-spec request(binary(), binary(), headers(), iodata(),
        pos_integer() | infinity, [option()]) -> result().

request(URL, Method, Hdrs, Body, Timeout, Options) ->
    {Host, Port, Path, Ssl} = dlhttpc_lib:parse_url(URL),
    request(Host, Port, Ssl, Path, Method, Hdrs, Body, Timeout, Options).

-spec request(binary(), 1..65535, true | false, binary(), binary(),
    headers(), iodata(), pos_integer(), [option()]) -> result().

request(Host, Port, Ssl, Path, Method, Hdrs, Body, Timeout, Options) ->
    ReqId = {self(), os:timestamp()},
    case dlhttpc_lib:lookup(stream_to, Options) of
        undefined ->
            Args = [ReqId, self(), Host, Port, Ssl, Path, Method, Hdrs, Body, Options],
            Pid = spawn_link(dlhttpc_client, request, Args),
            receive
                {response, ReqId, Pid, R} ->
                    R;
                {exit, ReqId, Pid, Reason} ->
                    exit(Reason);
                {'EXIT', Pid, Reason} ->
                    exit(Reason)
            after Timeout ->
                    kill_client(Pid)
            end;
        StreamTo ->
            Args = [ReqId, StreamTo, Host, Port, Ssl, Path, Method, Hdrs, Body, Options],
            Pid = spawn(dlhttpc_client, request, Args),
            {ReqId, Pid}
    end.

%% application callabcks
-spec start(normal | {takeover, node()} | {failover, node()}, any()) ->
    {ok, pid()}.

start(_, Opts) ->
    if is_list(Opts) -> dlhttpc_sup:start_link(Opts);
       true -> dlhttpc_sup:start_link()
    end.

-spec stop(any()) -> ok.

stop(_) ->
    ok.
