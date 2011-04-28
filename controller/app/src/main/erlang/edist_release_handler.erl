-module(edist_release_handler).
-behavior(gen_fsm).

-include_lib("stdlib/include/qlc.hrl").

-export([start_link/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	 code_change/4, terminate/3]).

-export([idle/2, update/2, overlapping_update/2]).

-record(state, {release, vsn, updater}).

-define(PROCESS_NAME(Rel), {n, l, {?MODULE, Rel}}).

get_pid(Rel) ->
    gproc:lookup_pid(?PROCESS_NAME(Rel)).

start_link(Rel) ->
    gen_fsm:start_link(?MODULE, [Rel], []).

init([Rel]) ->
    gproc:reg(?PROCESS_NAME(Rel)),
    edist_event_bus:subscribe({release, Rel}, []),
    {ok, idle, #state{release=Rel}}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, enotsup}, StateName, State}.

handle_info({edist_event_bus, Header, Msg}, StateName, State) ->
    Rel = State#state.release,
    {release, Rel} = proplists:get_value(type, Header),
    handle_bus(Msg, State),
    {next_state, StateName, State};
handle_info(_Msg, StateName, State) ->
    {next_state, StateName, State}.

handle_bus({commit, Vsn}, State) ->
    gen_fsm:send_event(get_pid(State#state.release), {update, Vsn});
handle_bus(_, _State) ->
    ok.

idle({update, Vsn}, State) ->
    Pid = spawn_link(fun() -> update_handler(State#state.release, Vsn) end),
    {next_state, update, State#state{updater=Pid, vsn=Vsn}}.

update({update, Vsn}, State) ->
    % This means someone tried to update the release while another update
    % was in progress.  This is fine, but we need to defer the deployment
    % until the current update completes
    {next_state, overlapping_update, State#state{vsn=Vsn}};
update(update_complete, State) ->
    {next_state, idle, State#state{updater=undefined}}.

overlapping_update({update, Vsn}, State) ->
    % see update(update, ..).
    {next_state, overlapping_update, State#state{vsn=Vsn}};
overlapping_update(update_complete, State) ->
    Pid = spawn_link(fun() ->
			     update_handler(State#state.release,
					    State#state.vsn)
		     end),
    {next_state, updating, State#state{updater=Pid}}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

% this process runs independently from the FSM during an update
update_handler(Rel, Vsn) ->
    Q = qlc:q([{Name, Pid} ||
		  {{p, g, {edist_release_subscriber, R}}, Pid, Name}
		      <- gproc:table(props),
		  R =:= Rel
	      ]),
    Subs = qlc:e(Q),

    lists:foreach(fun({Name, Pid}) ->
			  % ensure we are sufficently monitoring the target
			  erlang:monitor(process, Pid),
			  edist_event_bus:subscribe({subscriber, Name}, []),

			  % fire off an async event, which we will collect
			  % later
			  Pid ! {update_available, Vsn}
		  end,
		  Subs),

    % block until all clients have either loaded the new release, or 
    % have somehow died/disappeared
    Status = lists:map(fun({Name, Pid}) ->
			       R = collect_response(Name, Pid, {loaded, Vsn}),
			       {R, Name, Pid}
		       end,
		       Subs),

    % perform a rolling reload of each client that indicated it has
    % successfully loaded the new release
    lists:foreach(fun({ok, Name, Pid}) ->
			  Pid ! {reload, Vsn},
			  collect_response(Name, Pid, {online, Vsn});
		     ({_Status, _Name, _Pid}) ->
			  nop
		  end,
		  Status),

    % trip the state machine back to idle
    gen_fsm:send_event(get_pid(Rel), update_complete).

collect_response(Name, Pid, Event) ->
    receive
	{subscriber_event, Name, Event} ->
	    ok;
	{'DOWN', _Ref, process, Pid, _Info} ->
	    disconnect;
	{edist_event_bus, Header, Msg} ->
	    {subscriber, Id} = proplists:get_value(type, Header),
	    self() ! {subscriber_event, Id, Msg},
	    collect_response(Name, Pid, Event)
    after
	60000 ->
	    timeout
    end.
    

