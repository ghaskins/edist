-module(release_handler_tests).

-include_lib("eunit/include/eunit.hrl").

handler_test() ->
    Rel = "test",
    Vsn1 = "1.1",
    Vsn2 = "1.2",
    ClientName = "testclient",

    application:stop(gproc),
    application:set_env(gproc, gproc_dist, 'all'),
    application:start(gproc),

    {ok, Pid} = edist_release_handler:start_link(Rel),

    gproc:reg({p, g, {edist_release_subscriber, Rel}}, ClientName),
    edist_event_bus:notify({release, Rel}, {commit, Vsn1}),

    wait({update_available, Vsn1}),
    notify({loaded, Vsn1}, ClientName),
    
    % cause an overlapping update
    edist_event_bus:notify({release, Rel}, {commit, Vsn2}),
    
    wait({reload, Vsn1}),
    notify({online, Vsn1}, ClientName),
    
    wait({update_available, Vsn2}),
    notify({loaded, Vsn2}, ClientName),
    
    wait({reload, Vsn2}),
    notify({online, Vsn2}, ClientName),
    
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
