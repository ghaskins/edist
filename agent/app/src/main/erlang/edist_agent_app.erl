%% Author: ghaskins
-module(edist_agent_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    StartArgs = [{path, "/tmp/edist-agent"}],
    RequiredArgs = [path],

    lists:foldl(fun(Arg, _Acc) ->
                       case proplists:get_value(Arg, StartArgs) of
                           undefined -> throw({"Missing required arg", Arg});
                           _ -> ok
                       end
               end,
               void, RequiredArgs),

    Path = proplists:get_value(path, StartArgs),

    edist_agent_sup:start_link(Path).

stop(_State) ->
    ok.




