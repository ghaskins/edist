-module(event_logger).
-export([init/1, handle_event/2, terminate/2]).

init(_Args) ->
    {ok, []}.

handle_event(Event, State) ->
    error_logger:info_msg("Event: ~p~n", [Event]),
    {ok, State}.

terminate(_Args, _State) ->
    ok.
