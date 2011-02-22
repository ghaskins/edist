%% Author: ghaskins
-module(edist_agent_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, StartArgs) ->
    RequiredArgs = [app, path],

    lists:foldl(fun(Arg, _Acc) ->
			case proplists:get_value(Arg, StartArgs) of
			    undefined -> throw({"Missing required arg", Arg});
			    _ -> ok
			end
		end,
		void, RequiredArgs),

    App = proplists:get_value(app, StartArgs),
    Path = proplists:get_value(path, StartArgs),

    edist_agent_sup:start_link(App, Path).

stop(_State) ->
    ok.




