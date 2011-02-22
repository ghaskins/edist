%% Author: ghaskins
-module(edist_agent_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    edist_agent_sup:start_link().

stop(_State) ->
    ok.



