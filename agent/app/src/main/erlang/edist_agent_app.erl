%% Author: ghaskins
-module(edist_agent_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    case init:get_argument(pidfile) of
    {ok, [PidFile]} ->
        case file:write_file(PidFile, os:getpid()) of
	    ok -> ok;
	    Error ->
		io:format("Failed to write PID file ~s, error: ~p",
			  [PidFile, Error])
        end;
	_ -> ok
    end,

    RequiredArgs = [path],

    lists:foldl(fun(Arg, _Acc) ->
                       case application:get_env(Arg) of
                           undefined -> throw({"Missing required arg", Arg});
                           _ -> ok
                       end
               end,
               void, RequiredArgs),

    {ok, Path} = application:get_env(path),

    edist_agent_sup:start_link([Path]).

stop(_State) ->
    ok.




