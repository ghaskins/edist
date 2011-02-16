%% Author: ghaskins
-module(controller_app).
-behaviour(application).

-export([start/2, stop/1]).

start(Type, StartArgs) ->
    case controller_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(State) ->
    ok.



