%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(controller).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("release.hrl").

-export([start_link/0]).
-export([create_release/4, create_update/3, upload_release/4, commit_release/3]).
-export([close_stream/1]).
-export([inc_version/2, dec_version/2, rm_version/2, update_version/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(client, {cookie, pid, ref, joined=false, facts, rel}).
-record(state, {}).

api_version() -> 1.

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

create_release(Name, InitialVsn, Config, []) ->
    gen_server:call({global, ?MODULE}, {create_release, Name, InitialVsn, Config}).

create_update(Name, NextVsn, []) ->
    gen_server:call({global, ?MODULE}, {create_update, Name, NextVsn}).

upload_release(Name, Vsn, Criteria, []) ->
    gen_server:call({global, ?MODULE}, {upload_release, Name, Vsn, Criteria}).

commit_release(Name, Vsn, []) ->
    gen_server:call({global, ?MODULE}, {commit_release, Name, Vsn}).

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

handle_call({create_release, Name, InitialVsn, Config}, _From, State) ->
    F = fun() ->
		[] = mnesia:read(edist_releases, Name, write),
		Version = #edist_release_vsn{vsn=InitialVsn},
		Versions = dict:from_list([{InitialVsn, Version}]),
		Record = #edist_release{name=Name,
					config=Config,
					versions=Versions},
		mnesia:write(edist_releases, Record, write)
	end,
    try mnesia:transaction(F) of
	{atomic, ok} ->
	    {reply, ok, State}
    catch
	_:Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({create_update, Name, NextVsn}, _From, State) ->
    F = fun() ->
		[Record] = mnesia:read(edist_releases, Name, write),
		#edist_release_vsn{vsn=LatestVsn} = find_latest(Name),
		if
		    NextVsn =< LatestVsn  ->
			throw("Version must be greater than " ++ LatestVsn);
		    true ->
			ok
		end,
		Version = #edist_release_vsn{vsn=NextVsn},
		NewVersions = dict:store(NextVsn, Version,
					 Record#edist_release.versions),
		mnesia:write(edist_releases,
			     Record#edist_release{versions=NewVersions},
			     write)
	end,
    try mnesia:transaction(F) of
	{atomic, ok} ->
	    {reply, ok, State}
    catch
	_:Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({upload_release, Name, Vsn, Criteria}, _From, State) ->
    StartFunc = {release_input_device, start_link, [Name, Vsn, Criteria]},

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

handle_call({commit_release, Name, Vsn}, _From, State) ->
    F = fun() ->
		update_version(Name, Vsn,
			       fun(Version) ->
				       case Version#edist_release_vsn.state of
					   initializing  ->
					       Version#edist_release_vsn{
						 state=active
						};
					   _ ->
					       throw({"bad state", Version})
				       end
			       end),
		gen_event:notify({global, edist_event_bus},
				 {release_update, Name, Vsn}),

		Q = qlc:q([ Client || 
			      Client <- mnesia:table(edist_controller_clients),
			      Client#client.rel =:= Name
			  ]),
		lists:foreach(fun(Client) ->
				      Client#client.pid ! {update, Vsn}	      
			      end,
			      qlc:e(Q)),

		ok
	end,
    try mnesia:transaction(F) of
	{atomic, ok} ->
	    {reply, ok, State}
    catch
	_:Error ->
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

handle_call({client, join, Cookie, Facts, Rel}, _From, State) ->
    error_logger:info_msg("Client ~p: JOIN with facts ~p~n",
			   [Cookie, Facts]),
    
    F = fun() ->
		[Client] = mnesia:read(edist_controller_clients,
				       Cookie,
				       write),
		if
		    Client#client.joined =:= true ->
			throw("Already joined");
		    true -> ok
		end,

		#edist_release_vsn{vsn=Vsn} = find_latest_active(Rel),
		
		UpdatedClient = Client#client{joined=true,
					      facts=Facts,
					      rel=Rel}, 
		mnesia:write(edist_controller_clients, UpdatedClient, write),
		gen_event:notify({global, edist_event_bus},
				 {agent_join, Cookie, Facts}),
		{ok, Vsn}
	end,
    try mnesia:transaction(F) of
	{atomic, {ok, Vsn}} ->
	    {reply, {ok, Vsn}, State}
    catch
	_:Error ->
	    {reply, Error, State}
    end;

handle_call({client, download_release, Cookie}, {ClientPid, _Tag}, State) ->
    try
	F = fun() ->
		    [Client]
			= mnesia:read(edist_controller_clients, Cookie, read),
		    [#edist_release{config=Config}]
			= mnesia:read(edist_releases, Client#client.rel, read),

		    Version = find_latest_active(Client#client.rel),
		    {Client, Config, Version}
	    end,
	{atomic, {Client, Config, Version}} = mnesia:transaction(F),

	% perform a parallel search for any elements with matching criteria
	Parent = self(),
	Work = fun(#edist_release_elem{criteria=Criteria} = Element) ->
		       % FIXME: We are JIT'ing the criteria, might want to 
		       % precompile/cache somehow
		       {ok, Fun} = util:compile_native(Criteria),
		       case Fun(Client#client.facts) of
			   match ->
			       Parent ! {self(), {match, Element}};
			   nomatch ->
			       Parent ! {self(), nomatch}
		       end
	       end,

	Pids = [spawn_link(fun() -> Work(Element) end)
		|| Element <- Version#edist_release_vsn.elements],
	
	% grab the first matching reply since the list is in prio order
	case lists:foldl(fun(Pid, nomatch) ->
				 receive
				     {Pid, nomatch} -> nomatch; 
				     {Pid, {match, Element}} -> Element
				 end;
			    (Pid, Element) ->
				 receive
				     {Pid, _} -> Element
				 end
			 end,
			 nomatch,
			 Pids) of
	    nomatch ->
		throw("Matching criteria not found");
	    Element ->
		StartFunc = {release_output_device, start_link,
			     [Client#client.rel, Version, Element, ClientPid]},
		
		{ok, Dev} = controller_sup:start_child({erlang:now(),
							StartFunc,
							transient,
							brutal_kill,
							worker,
							[release_output_device]
						       }
						      ),

		{reply, {ok, Version#edist_release_vsn.vsn, Config, Dev}, State}
	end
    catch
	_:Error ->
	    {reply, {error, Error}, State}
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
		DelClient =
		    fun(Client) ->
			    gen_event:notify({global, edist_event_bus},
					     {agent_leave,
					      Client#client.cookie}),
			    mnesia:delete_object(Tab, Client, write)
		    end,

		Q = qlc:q([ DelClient(Client)
			   || Client <- mnesia:table(Tab),
			      Client#client.ref =:= Ref,
			      Client#client.pid =:= Pid
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

find_latest(Name) ->
    find_latest(Name, nofilter).

find_latest_active(Name) ->
    Filter = fun(Version) ->
		     Version#edist_release_vsn.state =:= active
	     end,
    find_latest(Name, Filter).
		    
find_latest(Name, Filter) ->
    [Record] = mnesia:read(edist_releases, Name, read),

    Versions = [Value ||
		   {_Key, Value}
		       <- dict:to_list(Record#edist_release.versions)
	       ],

    % find the latest VSN
    FilteredVersions = case Filter of
			   nofilter ->
			       Versions;
			   _ ->
			       lists:filter(Filter, Versions)
		       end,
    
    case lists:foldl(fun(Version, undefined) ->
			    Version;
		       (Version, AccIn)
			  when Version#edist_release_vsn.vsn >
			       AccIn#edist_release_vsn.vsn ->
			    Version;
		       (_Version, AccIn) ->
			    AccIn
		    end,
		    undefined,
		    FilteredVersions) of
	undefined ->
	    throw("No suitable version found");
	V -> V
    end.

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
    [Record] = mnesia:read(edist_releases, Name, write),
    NewVersions = dict:erase(Vsn, Record#edist_release.versions),
    case dict:size(NewVersions) of
	0 ->
	    ok = mnesia:delete_object(edist_releases,
				      Record, write);
	_ ->
	    NewRecord = Record#edist_release{versions=NewVersions},
	    ok = mnesia:write(edist_releases, NewRecord, write)
    end,
    
    % we remove all elements under vsn implictly
    Q = qlc:q([mnesia:delete_object(edist_release_blocks, R, write)
	       || R <- mnesia:table(edist_release_blocks),
		  R#edist_release_block.name == Name,
		  R#edist_release_block.vsn == Vsn
	      ]),
    qlc:e(Q),
    ok.
