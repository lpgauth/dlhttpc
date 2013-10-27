-module(dlhttpc_kill_manager).
-define(SERVER, ?MODULE).

-export([
    start_link/0
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% public
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({kill_client, Pid}, State) ->
    case is_process_alive(Pid) of
        true -> dlhttpc:kill_client(Pid);
        false -> ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
