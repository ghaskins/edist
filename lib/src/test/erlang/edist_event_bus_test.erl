-module(edist_event_bus_test).
-include_lib("eunit/include/eunit.hrl").

init_test() ->
    ok = application:start(gen_leader),
    ok = application:start(gproc).

subscribe_test() ->
    true = edist_event_bus:subscribe(foo, [{cookie, bar}, {scope, local}]),
    edist_event_bus:notify(foo, baz, [{scope, local}]),

    receive
	{edist_event_bus, _Header, baz} ->
	    ok;
	Msg -> throw({"Unexpected msg", Msg})
    after
	0 -> throw(timeout)
    end.

badoptions_test() ->
    try
	edist_event_bus:subscribe(bar, [{bar, baz}]),
	throw("unexpected success")
    catch
	throw:{earg, {bar, baz}} -> ok
    end.
	    
