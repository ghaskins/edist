-module(subagent).
-behavior(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {}).

start_link(Pid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Pid], []).

init([Pid]) ->
    erlang:monitor(process, Pid),
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, enotsup, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _Info}, State) ->
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->    
    {ok, State}.

terminate(_Reason, State) ->
    error_logger:info_msg("EDIST SubAgent: Connection to agent lost, terminating~n", []),
    init:stop(),
    ok.
