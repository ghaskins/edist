%% Author: ghaskins
-module(edist_controller_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    Nodes = try
		{ok, Props} = application:get_env(kernel, distributed),
		{ok, App} = application:get_application(),
		case proplists:get_value(App, Props) of
		    undefined ->
			throw(undefined);
		    Value ->
			Value
		end
	    catch
		_:_ -> [node()]
	    end,
		
    {ok, Pid} = edist_controller_sup:start_link(Nodes),
    gen_event:add_handler({global, edist_event_bus}, event_logger, []),

    {ok, Pid}.

stop(_State) ->
    ok.



