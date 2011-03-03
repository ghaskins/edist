%% Author: ghaskins
-module(controller_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case controller_sup:start_link() of
	{ok, Pid} ->
	    gen_event:add_handler({global, edist_event_bus}, event_logger, []),
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.



