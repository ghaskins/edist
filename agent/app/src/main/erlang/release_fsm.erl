-module(release_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/3, handle_info/3, handle_event/3,
	 handle_sync_event/4, code_change/4, terminate/3]).
-export([binding/2, disconnected_binding/2, upgrading_binding/2,
	 running/2, reloading/2, reconnecting/2]).

-include_lib("kernel/include/file.hrl").

-record(paths, {base, runtime}).
-record(state, {rel, vsn, paths, config,
		cname, cnode, session, rpid,
		tmoref
	       }).

start_link(Rel, BasePath, Session) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Rel, BasePath, Session], []).

init([Rel, BasePath, Session]) ->
    [Name, Host] = string:tokens(atom_to_list(node()), "@"),

    RuntimeDir = filename:join([BasePath, "runtime"]),

    case filelib:is_dir(RuntimeDir) of
	true ->
	    % make sure we dont have an old runtime hanging around
	    {ok, Files} = file:list_dir(RuntimeDir), 
	    target_system:remove_all_files(RuntimeDir, Files);
	false ->
	    ok = filelib:ensure_dir(RuntimeDir)
    end,

    ClientName = Name ++ "-" ++ Rel,
    ClientNode = list_to_atom(ClientName ++ "@" ++ Host),

    % Ensure that the client is not already running
    case net_adm:ping(ClientNode) of
	pong ->
	    rpc:call(ClientNode, init, stop, []);
	_ ->
	    void
    end,

    gproc:reg({p, g, {edist_release_subscriber, Rel}}, ClientName),
    notify({subscribe, Rel}, ClientName),

    launch(Session, #state{rel=Rel,
			   paths=#paths{base=BasePath, runtime=RuntimeDir}, 
			   cname=ClientName, cnode=ClientNode}).

binding({release_stopped, _Data}, State) ->
    gen_fsm:cancel_timer(State#state.tmoref),
    {stop, "Subordinate death", State};
binding({timeout, _, bind}, State) ->
    case bind(State) of
	true ->
	    {next_state, running, State};
	false ->
	    {next_state, binding, start_timer(1000, State)}
    end;
binding({update_available, Vsn}, State) ->
    if
	Vsn =/= State#state.vsn ->
	    {ok, upgrade_binding, State};
	true ->
	    {ok, binding, State}
    end;
binding({controller, disconnected}, State) ->
    {next_state, disconnected_binding, State#state{session=undefined}}.

disconnected_binding({release_stopped, _Data}, State) ->
    gen_fsm:cancel_timer(State#state.tmoref),
    {stop, "Subordinate death", State};
disconnected_binding({timeout, _, bind}, State) ->
    case bind(State) of
	true ->
	    {next_state, reconnecting, State};
	false ->
	    {next_state, disconnected_binding, start_timer(1000, State)}
    end;
disconnected_binding({update_available, _Vsn}, State) ->
    {next_state, disconnected_binding, State};
disconnected_binding({controller, connected, Session}, State) ->
    {ok, RelProps} =
	controller_api:query_release(Session, State#state.rel),
    
    NewState = State#state{session=Session},

    Vsn = get_prop(vsn, RelProps),
    if
	Vsn =:= NewState#state.vsn ->
	    {ok, binding, NewState};
	true ->
	    {ok, upgrading_binding, NewState}
    end.

upgrading_binding({release_stopped, _Data}, State) ->
    gen_fsm:cancel_timer(State#state.tmoref),
    {stop, "Subordinate death", State};
upgrading_binding({timeout, _, bind}, State) ->
    case bind(State) of
	true ->
	    reload(State);
	false ->
	    {next_state, upgrading_binding, start_timer(1000, State)}
    end;
upgrading_binding({update_available, _Vsn}, State) ->
    {next_state, upgrading_binding, State};
upgrading_binding({controller, disconnected}, State) ->
    {next_state, disconnected_binding,
     State#state{session=undefined}}.

running({release_stopped, _Data}, State) ->
    {stop, "Subordinate death", State};
running({update_available, Vsn}, State) ->
    download_latest(State#state.session, State),
    {next_state, running, State};
running({reload, Vsn}, State) ->
    if
	Vsn =:= State#state.vsn ->
	    notify({online, State#state.vsn}, State),
	    {ok, running, State};
	true ->
	    reload(State)
    end;
running({controller, disconnected}, State) ->
    {next_state, reconnecting, State#state{session=undefined}}.

reloading({release_stopped, _Data}, State) ->
    gen_fsm:cancel_timer(State#state.tmoref),
    {ok, NextStateName, NextState} = launch(State#state.session, State), 
    {next_state, NextStateName, NextState};
reloading({update_available, Vsn}, State) ->
    {next_state, reloading, State};
reloading(reload, State) ->
    {next_state, reloading, State}.

%FIXME: handle controller disconnects while reloading

reconnecting({release_stopped, _Data}, State) ->
    {stop, "Subordinate death", State};
reconnecting({controller, connected, Session}, State) ->
    {ok, RelProps} =
	controller_api:query_release(Session, State#state.rel),
    
    Vsn = get_prop(vsn, RelProps),

    NewState = State#state{session=Session},
    if
	Vsn =/= NewState#state.vsn ->
	    reload(NewState);
	true ->
	    {next_state, running, NewState}
    end.

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
    stop(State#state.cnode),
    error_logger:info_msg("Terminate: ~p~n", [Reason]);
terminate(Reason, _StateName, _State) ->
    error_logger:info_msg("Terminate: ~p~n", [Reason]),
    void.

start_timer(Tmo, State) ->
    TmoRef = gen_fsm:start_timer(Tmo, bind),
    State#state{tmoref=TmoRef}.

stop(Node) ->
    error_logger:info_msg("Stopping ~p~n", [Node]),
    rpc:call(Node, init, stop, []).   

get_prop(Prop, Props) ->
    case proplists:get_value(Prop, Props) of
	undefined -> throw({"Required property missing", Prop});
	V -> V
    end.

compute_sha(File) ->
    case filelib:is_file(File) of
	false ->
	    notpresent;
	true ->
	    {ok, Dev} = file:open(File, [read, binary]),
	    Sha = compute_sha(Dev, crypto:sha_init()),
	    file:close(Dev),
	    Sha
    end.

compute_sha(Dev, Ctx) ->
    case file:read(Dev, 4096) of
	{ok, Data} ->
	    compute_sha(Dev, crypto:sha_update(Ctx, Data));
	eof ->
	    crypto:sha_final(Ctx)
    end.

reload(State) ->
    stop(State#state.cnode),
    {next_state, reloading, start_timer(30000, State)}.  

download_latest(Session, State) ->
    ImageFile = filename:join([State#state.paths#paths.base,
			       "image.cache"]),

    {ok, RelProps} =
	controller_api:query_release(Session, State#state.rel),
    
    Config = get_prop(config, RelProps),
    
    {ok, ImageProps, IDev} =
	controller_api:download_release(Session, State#state.rel),
    
    Vsn = get_prop(vsn, ImageProps),
    ProvidedSha = proplists:get_value(sha, ImageProps),
    ComputedSha = compute_sha(ImageFile),

    Status =
	if ComputedSha =/= ProvidedSha ->
		{ok, ODev} = file:open(ImageFile, [write, binary]),
		{ok, _} = file:copy(IDev, ODev),
		file:close(ODev),
		
		% validate the integrity of the download
		ProvidedSha = compute_sha(ImageFile),
		
		downloaded;
	   true ->
		cached
	end,
    
    ok = controller_api:close_stream(IDev),

    notify({loaded, Vsn}, State),
    {ok, Status, Vsn, ImageFile, Config}.

launch(Session, State) ->
    {ok, DlState, Vsn, ImageFile, Config} = download_latest(Session, State),

    case DlState of
	downloaded ->
	    error_logger:info_msg("Downloading release file~n", []);
	cached ->
	    error_logger:info_msg("Using cached release file~n", [])
    end,

    RelName = relname(State#state.rel, Vsn),
    RuntimeDir = State#state.paths#paths.runtime,
    ok = target_system:install(RelName, RuntimeDir, ImageFile),
    
    BootFile = filename:join([RuntimeDir, "releases", Vsn, "start"]),
    Cmd = filename:join([RuntimeDir, "bin", "erl"]) ++ 
	" -boot " ++ BootFile ++
	" -noinput" ++
	" -sname " ++ State#state.cname ++
	" " ++ Config,
    
    error_logger:info_msg("Launching ~s~n", [Cmd]),
    
    S = self(),
    RPid = spawn_link(fun() ->
			      Result = try os_cmd:os_cmd(Cmd)
				       catch
					   throw:{badstatus, Status} ->
					       proplists:get_value(status, Status)
				       end,
			      gen_fsm:send_event(S, {release_stopped, Result})
		      end),
    
    NewState = State#state{session=Session,
			   vsn=Vsn, config=Config, rpid=RPid},
    case bind(NewState) of
	true ->
	    {ok, running, NewState};
	false ->
	    {ok, binding, start_timer(500, NewState)}
    end.

bind(State) ->
    error_logger:info_msg("Binding to ~p....~n", [State#state.cnode]), 
    case net_adm:ping(State#state.cnode) of
	pong ->
	    S = self(),
	    spawn(
	      State#state.cnode,
	      fun() ->
		      erlang:monitor(process, S),
		      receive
			  {'DOWN', _Ref, process, S, _Info} ->
			      ok
		      end,
		      error_logger:info_msg("EDIST: Lost connection~n", []),
		      init:stop()
	      end
	     ),
	    	    	
	    error_logger:info_msg("Binding complete~n", []),
	    notify({online, State#state.vsn}, State),
	    edist_event_bus:notify({release, State#state.rel},
				   {online, State#state.cnode}),
	    true;
	_ ->
	    false
    end.

relname(Rel, Vsn) ->
    Rel ++ "-" ++ Vsn.

os_cmd(Cmd) ->
    [Tmp | _ ] = string:tokens(os_cmd:os_cmd(Cmd), "\n"),
    Tmp.

tempfile() -> 
    os_cmd("mktemp").

notify(Event, State) when is_record(State, state) ->
    notify(Event, State#state.cname);
notify(Event, Id) ->
    edist_event_bus:notify({subscriber, Id}, Event).
