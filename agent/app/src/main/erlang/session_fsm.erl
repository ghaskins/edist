-module(session_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/1, handle_info/3, handle_event/3,
	 handle_sync_event/4, code_change/4, terminate/3]).
-export([connecting/2, connected/2]).

-include_lib("kernel/include/file.hrl").

-record(release, {name, pid}).
-record(state, {path, releases=dict:new(), cpid, session}).

start_link(Path) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Path], []).

init([Path]) ->
    Facts = facter:get_facts() ++
	[
	 {"api_version", controller_api:api_version()}
	],
    error_logger:info_msg("Starting with facts: ~p~n", [Facts]),

    Props = [{{edist_fact, K}, V} || {K, V} <- Facts],
    gproc:mreg(p, g, Props),
    gproc:reg({p, g, edist_client}),
 
    connect(),
    {ok, connecting, #state{path=Path}}.

connecting({connected, CPid}, State) ->
    erlang:monitor(process, CPid),
    gproc_dist:sync(),
    
    {ok, Session} = controller_api:negotiate(CPid),
    {ok, Properties} = controller_api:join(Session),
 
    RequiredSet = case proplists:get_value(releases, Properties) of
			   undefined -> [];
			   V -> sets:from_list(V)
		       end,

    State1 = State#state{cpid=CPid, session=Session},
    bcast_event({controller, connected, Session}, State1),
    {ok, State2} = update_releases(RequiredSet, State1),
    {next_state, connected, State2}.

connected({update_releases, Releases}, State) ->
    {next_state, connected, update_releases(sets:from_list(Releases), State)};
connected({'DOWN', _Ref, process, Pid, _Info}, State) when Pid =:= State#state.cpid ->
    bcast_event({controller, disconnected}, State),
    connect(),
    {next_state, connecting, State#state{cpid=undefined, session=undefined}}.

handle_event(Event, _StateName, _State) ->
    throw({"Unexpected event", Event}).

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, einval}, StateName, State}.

handle_info({gproc, _Ref, registered, {edist_controller, CPid, _Value}}, StateName, State) ->
    send_connected(CPid),
    {next_state, StateName, State};
handle_info(Info, StateName, State) ->
    gen_fsm:send_event(self(), Info),
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, _StateName, _State) ->
    error_logger:info_msg("Terminate: ~p~n", [Reason]),
    void.

bcast_event(Event, State) ->
    lists:foreach(fun({_, #release{pid=Pid}}) ->
			  gen_fsm:send_event(Pid, Event)
		  end,
		  dict:to_list(State#state.releases)).
    
update_releases(RequiredSet, State) ->
    CurrentReleases = dict:fetch_keys(State#state.releases),

    CurrentSet = sets:from_list(CurrentReleases),

    AddReleases = sets:to_list(sets:subtract(RequiredSet, CurrentSet)),
    DropReleases = sets:to_list(sets:subtract(CurrentSet, RequiredSet)),

    State1 = lists:foldl(fun(Rel, Acc) ->
				 Id = list_to_atom(Rel),
				 Module = release_fsm,

				 BasePath = filename:join([State#state.path, "releases", Rel]),
				 StartFunc = {Module, start_link,
					      [Rel, BasePath, State#state.session]},
				 
				 {ok, Pid} = edist_agent_sup:start_child({Id,
									  StartFunc,
									  transient,
									  brutal_kill,
									  worker,
									  [Module]
									 }
									),
				 Release = #release{name=Rel, pid=Pid},
				 Releases = dict:store(Rel, Release, Acc#state.releases),
				 Acc#state{releases=Releases}
			 end,
			 State,
			 AddReleases),
    State2 = lists:foldl(fun(Rel, Acc) ->
				 Id = list_to_atom(Rel),
				 ok = edist_agent_sup:delete_child(Id),

				 Releases = dict:erase(Rel, Acc#state.releases),
				 Acc#state{releases=Releases}
			 end,
			 State1,
			 DropReleases),

    {ok, State2}.

connect() ->
    case gproc:nb_wait({n, g, edist_controller}) of
	{_Ref, {{n, g, edist_controller}, Pid, _}} ->
	    send_connected(Pid);
	_Ref ->
	    ok
    end.

send_connected(CPid) ->
    gen_fsm:send_event(self(), {connected, CPid}).

