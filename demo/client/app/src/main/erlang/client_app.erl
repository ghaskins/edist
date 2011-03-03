%% Author: ghaskins
-module(client_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    client_sup:start_link().

stop(_State) ->
    ok.



