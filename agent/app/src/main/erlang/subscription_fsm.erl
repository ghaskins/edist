-module(subscription_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/1, handle_info/3, handle_event/3,
	 handle_sync_event/4, code_change/4, terminate/3]).
-export([connecting/2, assigning/2, binding/2, running/2, reconnecting/2]).

-include_lib("kernel/include/file.hrl").

-record(paths, {runtime}).
-record(state, {rel, vsn, paths, facts, config,
		cpid, cname, cnode, session, rpid,
		tmoref
	       }).

start_link(Path) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Path], []).

init([Path]) ->
    Facts = facter:get_facts(),

    [Name, Host] = string:tokens(atom_to_list(node()), "@"),

    ClientName = Name ++ "-client",
    ClientNode = list_to_atom(ClientName ++ "@" ++ Host),

    % Ensure that the client is not already running
    case net_adm:ping(ClientNode) of
	pong ->
	    rpc:call(ClientNode, init, stop, []);
	_ ->
	    void
    end,

    RuntimeDir = filename:join([Path, "runtime"]),

    case filelib:is_dir(RuntimeDir) of
	true ->
	    % make sure we dont have an old runtime hanging around
	    {ok, Files} = file:list_dir(RuntimeDir), 
	    target_system:remove_all_files(RuntimeDir, Files);
	false ->
	    ok = filelib:ensure_dir(RuntimeDir)
    end,

    error_logger:info_msg("Starting with facts: ~p~n", [Facts]),
    Paths = #paths{runtime=RuntimeDir},
    {ok, connecting, #state{paths=Paths, facts=Facts,
			    cname=ClientName, cnode=ClientNode}}.

connecting({controller, connected, Pid}, State) ->
    {ok, Session} = controller_api:negotiate(Pid),
    ok = controller_api:join(Session, State#state.facts),
 
    {next_state, assigning, State#state{cpid=Pid, session=Session}}.
    
assigning({assignment, Rel, Config}, State) ->
    TmpFile = tempfile(),

    try
	{ok, Vsn, IDev} =
	    controller_api:download_release(State#state.session),
	ok = remote_copy(IDev, TmpFile),
	
	RelName = relname(Rel, Vsn),
	RuntimeDir = State#state.paths#paths.runtime,
	ok = target_system:install(RelName, RuntimeDir, TmpFile),

	BootFile = filename:join([RuntimeDir, "releases", Vsn, "start"]),
	Cmd = filename:join([RuntimeDir, "bin", "erl"]) ++ 
	    " -boot " ++ BootFile ++
	    " -noinput" ++
	    " -sname " ++ State#state.cname ++
	    " " ++ Config,

	error_logger:info_msg("Launching ~s~n", [Cmd]),

	S = self(),
	Pid = spawn_link(fun() ->
				 Result = os_cmd:os_cmd(Cmd),
				 gen_fsm:send_event(S, {release_stopped, Result})
			 end),

	TmoRef = gen_fsm:start_timer(500, bind),
	{next_state, binding,
	 State#state{rel=Rel, vsn=Vsn, config=Config, rpid=Pid, tmoref=TmoRef}}
    after
	ok = file:delete(TmpFile)
    end;
assigning({controller, disconnected}, State) ->
    {next_state, connecting, State#state{cpid=undefined, session=undefined}}.

binding({release_stopped, _Data}, State) ->
    gen_fsm:cancel_timer(State#state.tmoref),
    {stop, normal, State};
binding({timeout, _, bind}, State) ->
    error_logger:info_msg("Binding to ~p....~n", [State#state.cnode]), 
    case net_adm:ping(State#state.cnode) of
	pong ->
	    error_logger:info_msg("Binding complete~n", []),
	    gen_event:notify({global, edist_event_bus},
			     {online, State#state.cnode}),
	    {next_state, running, State};
	_ ->
	    TmoRef = gen_fsm:start_timer(1000, bind),
	    {next_state, binding, State#state{tmoref=TmoRef}}
    end.

running({release_stopped, _Data}, State) ->
    {stop, normal, State};
running({hotupdate, _Vsn}, _State) ->
    ok;
running({controller, disconnected}, State) ->
    {next_state, reconnecting, State#state{cpid=undefined, session=undefined}}.

reconnecting({controller, connected, Pid}, State) ->
    {ok, Session} = controller_api:negotiate(Pid),
    ok = controller_api:rejoin(Session, State#state.facts,
			       State#state.rel),

    {ok, running, State#state{cpid=Pid, session=Session}}.

handle_event(Event, _StateName, _State) ->
    throw({"Unexpected event", Event}).

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, {error, einval}, StateName, State}.

handle_info(Info, StateName, State) ->
    gen_fsm:send_event(self(), Info),
    {next_state, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(Reason, running, State) ->
    rpc:call(State#state.cnode, init, stop, []),
    error_logger:info_msg("Terminate: ~p~n", [Reason]);
terminate(Reason, _StateName, _State) ->
    error_logger:info_msg("Terminate: ~p~n", [Reason]),
    void.

relname(Rel, Vsn) ->
    Rel ++ "-" ++ Vsn.

remote_copy(IDev, File) ->
    {ok, ODev} = file:open(File, [write, binary]),
    {ok, _} = file:copy(IDev, ODev),
    file:close(ODev),
    controller_api:close_stream(IDev).   

os_cmd(Cmd) ->
    [Tmp | _ ] = string:tokens(os_cmd:os_cmd(Cmd), "\n"),
    Tmp.

tempfile() -> 
    os_cmd("mktemp").

