%% Author: ghaskins
-module(edist_agent_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    RequiredArgs = [path],

    lists:foldl(fun(Arg, _Acc) ->
                       case application:get_env(Arg) of
                           undefined -> throw({"Missing required arg", Arg});
                           _ -> ok
                       end
               end,
               void, RequiredArgs),

    {ok, Path} = application:get_env(path),

    edist_agent_sup:start_link(Path).

stop(_State) ->
    ok.




