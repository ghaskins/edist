-module(subscription_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/0, handle_info/3, terminate/3]).
-export([connecting/2, assigning/2, running/2, reconnecting/2]).

-include_lib("kernel/include/file.hrl").

-record(state, {rel, vsn, path, facts, config, pid, session}).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    Path = tempdir(),
    Facts = facter:get_facts(),

    error_logger:info_msg("Starting with facts: ~p~n", [Facts]),

    {ok, connecting, #state{path=Path, facts=Facts}}.

connecting({controller, connected, Pid}, State) ->
    {ok, Session} = controller_api:negotiate(Pid),
    ok = controller_api:join(Session, State#state.facts),
 
    {next_state, assigning, State#state{pid=Pid, session=Session}}.
    
assigning({assignment, Rel, Config}, State) ->
    TmpFile = tempfile(),

    try
	{ok, Vsn, IDev} =
	    controller_api:download_release(State#state.session, Rel),
	ok = remote_copy(IDev, TmpFile),
	
	RelName = relname(Rel, Vsn),
	ok = target_system:install(RelName, State#state.path, TmpFile),

	{next_state, running, State#state{rel=Rel, vsn=Vsn, config=Config}}
    after
	ok = file:delete(TmpFile)
    end;
assigning({controller, disconnected}, State) ->
    {next_state, connecting, State#state{pid=undefined, session=undefined}}.

running({hotupdate, Vsn}, State) ->
    ok;
running({controller, disconnected}, State) ->
    {next_state, reconnecting, State#state{pid=undefined, session=undefined}}.

reconnecting({controller, connected, Pid}, State) ->
    {ok, Session} = controller_api:negotiate(Pid),
    ok = controller_api:rejoin(Session, State#state.facts,
			       State#state.rel),

    {ok, running, State#state{pid=Pid, session=Session}}.

handle_info(Info, StateName, State) ->
    gen_fsm:send_event(self(), Info),
    {next_state, StateName, State}.

terminate(Reason, StateName, State) ->
    error_logger:info_msg("Terminate: ~p~n", [Reason]),
    cleanup(State),
    void.

relname(Rel, Vsn) ->
    Rel ++ "-" ++ Vsn.

remote_copy(IDev, File) ->
    {ok, ODev} = file:open(File, [write, binary]),
    {ok, _} = file:copy(IDev, ODev),
    file:close(ODev),
    controller_api:close_stream(IDev).   

cleanup(State) ->
    error_logger:info_msg("Cleaning up target~n", []),
    {ok, Files} = file:list_dir(State#state.path), 
    target_system:remove_all_files(State#state.path, Files),
    file:del_dir(State#state.path).

tempfile() ->   
    os_cmd:os_cmd("mktemp").

tempdir() ->   
    os_cmd:os_cmd("mktemp -d").
