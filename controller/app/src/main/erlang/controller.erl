%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(controller).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("release.hrl").

-export([start_link/0, install_release/3, read_release/2, close_stream/1]).
-export([inc_version/2, dec_version/2, rm_version/2, update_version/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(client, {cookie, pid, ref, app=undefined}).
-record(state, {}).

api_version() -> 1.


%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

install_release(Name, Vsn, []) ->
    gen_server:call({global, ?MODULE}, {install_release, Name, Vsn}).

read_release(Name, Vsn) ->
    gen_server:call({global, ?MODULE}, {read_release, Name, Vsn}).

close_stream(IoDevice) ->
    gen_server:call(IoDevice, close).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    ok = util:open_table(edist_releases,
    			 [
    			  {record_name, edist_release},
    			  {attributes,
    			   record_info(fields, edist_release)}
    			 ]),
    
    ok = util:open_table(edist_release_blocks,
    			 [
    			  {record_name, edist_release_block},
    			  {attributes,
    			   record_info(fields, edist_release_block)}
    			 ]),
    
    mnesia:delete_table(edist_controller_clients),
    ok = util:open_table(edist_controller_clients,
    			 [
    			  {record_name, client},
    			  {attributes, record_info(fields, client)}
    			 ]),
    
    {ok, #state{}}.

handle_call({install_release, Name, Vsn}, _From, State) ->
    StartFunc = {release_input_device, start_link, [Name, Vsn]},

    case controller_sup:start_child({erlang:now(),
				     StartFunc,
				     transient,
				     brutal_kill,
				     worker,
				     [release_input_device]}) of
	{ok, Pid} ->
	    {reply, {ok, Pid}, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({read_release, Name, Vsn}, _From, State) ->
    try
	F = fun() ->
		    [Record] = mnesia:read(edist_releases, Name, read),
		    dict:fetch(Vsn, Record#edist_release.versions)
			
	    end,
	{atomic, Version} = mnesia:transaction(F),
	{ok, Pid} = open(Name, Version),
	{reply, {ok, Pid}, State}
    catch
	Type:Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({client, open_latest, Cookie}, _From, State) ->
    error_logger:info_msg("Client ~p: OPEN_LATEST~n",[Cookie]),
    
    try
	{ok, Client} = cookie2client(Cookie),
	Name = Client#client.app,
	F = fun() ->
		    [Record] = mnesia:read(edist_releases, Name, read),
		    
		    % find the latest VSN
		    case dict:fold(fun(Key, _Value, undefined) ->
					   Key;
				      (Key, _Value, AccIn) when Key > AccIn ->
					   Key;
				      (Key, _Value, AccIn) ->
					   AccIn
				   end,
				   undefined,
				   Record#edist_release.versions) of
			undefined ->
			    throw("No suitable version found");
			Vsn ->
			    dict:fetch(Vsn, Record#edist_release.versions)
		    end
	    end,
	{atomic, Version} = mnesia:transaction(F),
	{ok, Pid} = open(Name, Version),
	{reply, {ok, Version#edist_release_vsn.vsn, Pid}, State}
   catch
	Type:Error ->
	    {reply, {error, Error}, State}
   end;

handle_call({client, negotiate, ApiVsn, Args}, {Pid,_Tag}, State) ->
    ApiVsn = api_version(),
    Cookie = erlang:now(),

    error_logger:info_msg("Client ~p(~p): NEGOTIATE with Api: ~p Options: ~p~n",
			   [Cookie, Pid, ApiVsn, Args]),
    
    F = fun() ->
		Ref = erlang:monitor(process, Pid),
		Client = #client{cookie=Cookie, pid=Pid, ref=Ref},
		mnesia:write(edist_controller_clients, Client, write)
	end,
    {atomic, ok} = mnesia:transaction(F),
    {reply, {ok, ApiVsn, [], Cookie}, State};

handle_call({client, subscribe, Cookie, App}, From, State) ->
    error_logger:info_msg("Client ~p: SUBSCRIBE to app ~s~n",
			   [Cookie, App]),
    
    F = fun() ->
		[Client] = mnesia:read(edist_controller_clients, Cookie, write),
		mnesia:write(edist_controller_clients, Client#client{app=App}, write)
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    {reply, ok, State};
	Error ->
	    {reply, Error, State}
    end;
		    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'DOWN', Ref, process, Pid, _Info}, State) ->
    error_logger:info_msg("Client ~p: DOWN~n", [Pid]),
    
    Tab = edist_controller_clients,
    F = fun() ->
		Q = qlc:q([mnesia:delete_object(Tab, R, write)
			   || R <- mnesia:table(Tab),
			      R#client.ref =:= Ref,
			      R#client.pid =:= Pid
			  ]),
		qlc:e(Q),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Helper functions
%% --------------------------------------------------------------------

cookie2client(Cookie) ->
    F = fun() ->
		[Client] = mnesia:read(edist_controller_clients, Cookie, read),
		{ok, Client}
	end,
    {atomic, Ret} = mnesia:transaction(F),
    Ret.

open(Name, Version) ->
    StartFunc = {release_output_device, start_link, [Name, Version]},
	    
    controller_sup:start_child({erlang:now(),
				StartFunc,
				transient,
				brutal_kill,
				worker,
				[release_output_device]}).
     
inc_version(Name, Vsn) ->
    update_version(Name, Vsn, fun(Version) ->
				      Refs = Version#edist_release_vsn.ref_count,
				      Version#edist_release_vsn{
					ref_count=Refs+1
				       }
			      end).
   
dec_version(Name, Vsn) ->
    update_version(Name, Vsn, fun(Version) ->
				      Refs = Version#edist_release_vsn.ref_count,
				      Version#edist_release_vsn{
					ref_count=Refs-1
				       }
			      end).  

update_version(Name, Vsn, Fun) ->
    [Record] = mnesia:read(edist_releases, Name, write),
    {ok, Version} = dict:find(Vsn, Record#edist_release.versions),
    NewVersion = Fun(Version),
    NewVersions = dict:store(Vsn, NewVersion,
			     Record#edist_release.versions),
    NewRecord = Record#edist_release{versions=NewVersions},
    mnesia:write(edist_releases, NewRecord, write),
    {ok, NewVersion}.

rm_version(Name, Vsn) ->
    Record = case mnesia:read(edist_releases, Name, write) of
		 [] -> #edist_release{name=Name};
		 [R] -> R
	     end,
    NewVersions = dict:erase(Vsn, Record#edist_release.versions),
    case dict:size(NewVersions) of
	0 ->
	    ok = mnesia:delete_object(edist_releases,
				      Record, write);
	_ ->
	    NewRecord = Record#edist_release{versions=NewVersions},
	    ok = mnesia:write(edist_releases, NewRecord, write)
    end,
    
    Q = qlc:q([mnesia:delete_object(edist_release_blocks, R, write)
	       || R <- mnesia:table(edist_release_blocks),
		  R#edist_release_block.name == Name,
		  R#edist_release_block.vsn == Vsn
	      ]),
    qlc:e(Q),
    ok.
