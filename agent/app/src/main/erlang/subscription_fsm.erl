-module(subscription_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/2, terminate/3]).
-export([connecting/2, reconnecting/2, connected/2]).

-include_lib("kernel/include/file.hrl").

-record(state, {app, vsn, path, pid, session}).

start_link(App, Path) ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [App, Path], []).

init([App, Path]) ->
    case file:read_file_info(Path) of
	{ok, #file_info{type=directory}} -> ok;
	_ ->
	    throw({"Path does not exist", Path})
    end,

    State = #state{app=App, path=Path},
    try
	RELEASES = filename:join([Path, "releases", "RELEASES"]),
	{ok, _} = file:read_file_info(RELEASES),
	Vsn = case target_system:get_ertsvsn(Path) of
		  {ok, ErtsVsn, RelVsn} -> RelVsn
	      end,
	error_logger:info_msg("[~s] Found previous version: ~s~n", [App, Vsn]),
	{ok, reconnecting, State#state{vsn=Vsn}}
    catch
	_:_ ->
	    error_logger:info_msg("[~s] Resetting target~n", [App]),
	    {ok, Files} = file:list_dir(Path), 
	    target_system:remove_all_files(Path, Files),
	    {ok, connecting, State#state{vsn=undefined}}
    end.

connecting({controller, connected, Pid}, State) ->
    {ok, Session} = llconnect(State#state.app, Pid),
    {ok, Vsn, IDev} = controller_api:open_latest(Session),
    TmpFile = mktemp(),
    {ok, ODev} = file:open(TmpFile, [write, binary]),
    {ok, _} = file:copy(IDev, ODev),
    file:close(ODev),
    controller_api:close_stream(IDev),

    ok = target_system:install(State#state.app, State#state.path, TmpFile),
    ok = file:delete(TmpFile),

    {next_state, connected, State#state{pid=Pid, session=Session, vsn=Vsn}}.

reconnecting({controller, connected, Pid}, State) ->
    reconnect(Pid, State).

connected({controller, disconnected}, State) ->
    {next_state, connecting, State#state{pid=undefined, session=undefined}}.

% low-level connect
llconnect(App, Pid) ->
    {ok, Session} = controller_api:negotiate(Pid),
    ok = controller_api:subscribe(Session, App),
    error_logger:info_msg("Connected to controller~n"),
    {ok, Session}.

reconnect(Pid, State) ->
    {ok, Session} = llconnect(State#state.app, Pid),
    {next_state, connected, State#state{pid=Pid, session=Session}}.    

terminate(_Reason, State, _Data) ->
    void.

mktemp() ->   
    os_cmd:os_cmd("mktemp").
