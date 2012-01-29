-module(release_handler_tests).

-include_lib("eunit/include/eunit.hrl").

initialize_test() ->
    application:stop(gproc),
    application:set_env(gproc, gproc_dist, 'all'),
    application:start(gproc).

handler_test() ->
    Rel = "test",
    Vsn1 = "1.1",
    Vsn2 = "1.2",
    ClientName = "testclient",

    {ok, Pid} = edist_release_handler:start_link(Rel),

    gproc:reg({p, g, {edist_release_subscriber, Rel}}, ClientName),
    edist_event_bus:notify({release, Rel}, {commit, Vsn1}),

    handle_update(Vsn1, ClientName),

    % cause an overlapping update
    edist_event_bus:notify({release, Rel}, {commit, Vsn2}),

    handle_reload(Vsn1, ClientName),

    handle_update(Vsn2, ClientName),
    handle_reload(Vsn2, ClientName),

    ok.

handle_update(Vsn, ClientName) ->
    wait({update_available, Vsn}),
    notify({loaded, Vsn}, ClientName).

handle_reload(Vsn, ClientName) ->
    wait({reload, Vsn}),
    notify({online, Vsn}, ClientName).

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
