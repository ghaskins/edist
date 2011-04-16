-module(event_logger).
-behavor(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Args) ->
    edist_event_bus:subscribe([]),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, {error, enotsup}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({edist_event_bus, Header, Data}=Event, State) ->
    error_logger:info_msg("Event: ~p~n", [Event]),
    {noreply, State}.

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
