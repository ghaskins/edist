-module(connection_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/0, handle_info/3, handle_sync_event/4, terminate/3]).
-export([connecting_agent/2, connecting_controller/2, connected/2]).
-export([get_state/0]).

-record(state, {pid, ref, tmo_ref}).

get_state() ->
    gen_fsm:sync_send_all_state_event(?MODULE, get_state).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    S = self(),
    genevent_bridge:add_handler(agent_link_events, S,
				fun(Event, _State) ->
					gen_fsm:send_event(S, {agent_link, Event})
				end,
				[]),

    State = #state{},
    case agent_link:get_state() of
	{ok, connected} ->
	    connect_controller(ok, State);
	{ok, disconnected} ->
	    {ok, connecting_agent, State}
    end.

connecting_agent({agent_link, connected}, State) ->
    connect_controller(State).

connecting_controller({timeout, _, retry_connection}, State) ->
    connect_controller(State);
connecting_controller({agent_link, connected}, State) ->
    % no-op
    {next_state, connecting_controller, State};
connecting_controller({agent_link, disconnected}, State) ->
    gen_fsm:cancel_timer(State#state.tmo_ref),
    {next_state, connecting_agent, State#state{tmo_ref=undefined}}.

connected({agent_link, connected}, State) ->
    % no-op
    {next_state, connected, State};
connected({agent_link, disconnected}, State) ->
    NewState = disconnect_controller(State),
    {next_state, disconnected, NewState}.

handle_info({'DOWN', Ref, process, _Pid, _Info}, connected, State)
  when Ref =:= State#state.ref ->
    NewState = disconnect_controller(State),
    connect_controller(NewState).

connect_controller(State) ->
    connect_controller(next_state, State).

connect_controller(Tag, State) ->
    case global:whereis_name(controller) of
	undefined ->
	    TmoRef = gen_fsm:start_timer(1000, retry_connection),
	    {Tag, connecting_controller, State#state{tmo_ref=TmoRef}};
	Pid ->
	    Ref = erlang:monitor(process, Pid),
	    gen_fsm:send_event(subscription_fsm, {controller, connected, Pid}),
	    {Tag, connected, State#state{pid=Pid, ref=Ref}}
    end.

handle_sync_event(get_state, _From, connected, State) ->
    Pid = State#state.pid,
    {reply, {ok, connected, Pid}, connected, State};
handle_sync_event(get_state, _From, StateName, State) ->
    {reply, {ok, disconnected}, StateName, State}.

disconnect_controller(State) ->
    gen_fsm:send_event(subscription_fsm, {controller, disconnected}),
    true = erlang:demonitor(State#state.ref, [flush]),
    State#state{pid=undefined, ref=undefined}.

terminate(_Reason, _State, _Data) ->
    genevent_bridge:delete_handler(agent_link_events, self()),
    void.
