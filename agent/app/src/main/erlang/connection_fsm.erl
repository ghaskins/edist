-module(connection_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/0, handle_info/3, terminate/3]).
-export([connecting_agent/2, connecting_controller/2, connected/2]).

-record(state, {pid, ref}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init(_Args) ->
    S = self(),
    genevent_bridge:add_handler(agent_link_events, S,
				fun(Event, _State) ->
					gen_fsm:send_event(S, {agent_link, Event})
				end,
				[]),
    {ok, connecting_agent, #state{}}.

connecting_agent({agent_link, connected}, State) ->
    {next_state, connecting_controller, State, 0}.

connecting_controller(timeout, State) ->
    case connect_controller() of
	not_found ->
	    {next_state, connecting_controller, State, 1000};
	{ok, Pid, Ref} ->
	    gen_fsm:send_event(subscription_fsm, {controller, connected, Pid}),
	    {next_state, connected, State#state{pid=Pid, ref=Ref}}
    end;
connecting_controller({agent_link, disconnected}, State) ->
    {next_state, connecting_agent, State}.

connected({agent_link, disconnected}, State) ->
    NewState = disconnect_controller(State),
    {next_state, disconnected, NewState}.

handle_info({'DOWN', Ref, process, _Pid, _Info}, connected, State)
  when Ref =:= State#state.ref ->
    NewState = disconnect_controller(State),
    {next_state, connecting_controller, NewState, 1000}.

connect_controller() ->
    case global:whereis_name(controller) of
	undefined ->
	    not_found;
	Pid ->
	    Ref = erlang:monitor(process, Pid),
	    {ok, Pid, Ref}
    end.

disconnect_controller(State) ->
    gen_fsm:send_event(subscription_fsm, {controller, disconnected}),
    true = erlang:demonitor(State#state.ref, [flush]),
    State#state{pid=undefined, ref=undefined}.

terminate(_Reason, State, _Data) ->
    genevent_bridge:delete_handler(agent_link_events, self()),
    void.
