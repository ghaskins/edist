-module(release_handler_tests).

-include_lib("eunit/include/eunit.hrl").

handler_test() ->
    Rel = "test",
    Vsn = "1.0",
    ClientName = "testclient",

    application:stop(gproc),
    application:set_env(gproc, gproc_dist, 'all'),
    application:start(gproc),

    {ok, Pid} = edist_release_handler:start_link(Rel),

    gproc:reg({p, g, {edist_release_subscriber, Rel}}, ClientName),
    edist_event_bus:notify({release, Rel}, {commit, Vsn}),

    wait({update_available, Vsn}),
    notify({loaded, Vsn}, ClientName),
    
    wait({reload, Vsn}),
    notify({online, Vsn}, ClientName),
    
    ok.


wait(Event) ->
    receive
	Event ->
	    ok
    after
	2000 ->
	    throw(timeout)
    end.

notify(Event, Id) ->
    edist_event_bus:notify({subscriber, Id}, Event).
