%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(edist_controller).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("release.hrl").
-include_lib("groups.hrl").

-export([start_link/1]).
-export([create_release/4, create_update/3, upload_release/4, commit_release/3]).
-export([close_stream/1]).
-export([create_group/4]).
-export([inc_version/2, dec_version/2, rm_version/2, update_version/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(client, {cookie, pid, ref, joined=false, facts}).
-record(state, {}).

-define(RECORD(R), {R, record_info(fields, R)}).

api_version() -> 1.

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Nodes) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Nodes], []).

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

create_group(Name, Criteria, Releases, []) ->
    gen_server:call({global, ?MODULE},
		    {create_group, Name, Criteria, Releases}).

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
init([Nodes]) ->
    {ok, App} = application:get_application(),
    error_logger:info_msg("~p starting with replica set ~p~n", [App, Nodes]),

    ok = open_table(edist_releases, ?RECORD(edist_release), Nodes),
    ok = open_disc_table(edist_release_blocks, ?RECORD(edist_release_block), Nodes),
    
    ok = open_table(edist_groups, ?RECORD(edist_group), Nodes),
    mnesia:delete_table(edist_controller_clients),
    ok = open_ram_table(edist_controller_clients, ?RECORD(client), [node()]),
  
    % issue a compensating transaction to remove any dangling releases
    F = fun() ->
		CheckRel =
		    fun(Rel) ->
			    Dict = dict:filter(fun(_, Vsn) ->
						       active_vsn(Vsn) =:= false
					       end,
					       Rel#edist_release.versions), 
			    [{Rel#edist_release.name, Vsn#edist_release_vsn.vsn} ||
				{_, Vsn} <- dict:to_list(Dict)]
		    end,

		Q = qlc:q([ CheckRel(Rel) || Rel <- mnesia:table(edist_releases)]),
		lists:foreach(fun({Name, Vsn}) ->
				      rm_version(Name, Vsn)
			      end,
			      lists:flatten(qlc:e(Q))),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),

    {ok, #state{}}.

open_ram_table(Table, RecordInfo, Nodes) ->
    open_table(Table, ram_copies, RecordInfo, Nodes). 

open_disc_table(Table, RecordInfo, Nodes) ->
    open_table(Table, disc_only_copies, RecordInfo, Nodes). 

open_table(Table, RecordInfo, Nodes) ->
    open_table(Table, disc_copies, RecordInfo, Nodes). 

open_table(Table, Type, {Record, Info}, Nodes) ->
    util:open_table(Table,
		    [
		     {record_name, Record},
		     {attributes, Info},
		     {Type, Nodes}
		    ]).

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

    case edist_controller_sup:start_child({erlang:now(),
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
		edist_event_bus:notify(edist_releases, {update, Name, Vsn}),
		ok
	end,
    try mnesia:transaction(F) of
	{atomic, ok} ->
	    {reply, ok, State}
    catch
	_:Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({create_group, Name, Criteria, Releases}, _From, State) ->
    F = fun() ->
		{ok, _} = util:compile_native(Criteria),
		[] = mnesia:read(edist_groups, Name, write),
		Group = #edist_group{name=Name,
				     criteria=Criteria,
				     releases=Releases},
		mnesia:write(edist_groups, Group, write),
		
		lists:foreach(fun(Rel) ->
				      [_] = mnesia:read(edist_releases, Rel, read)
			      end,
			      Releases),

		Criterion = get_criterion(),
		Q = qlc:q([Client || Client <- mnesia:table(edist_controller_clients)]),

		% perform a parallel search for any existing clients that might
		% match the criteria of the group being created
		util:pmap(fun(#client{facts=Facts} = Client) ->
				  case exec_criteria(Facts, Criteria) of
				      match ->
					  {ok, R} = find_matching_releases(Facts, Criterion),
					  Client#client.pid ! {update_releases, R},
					  ok;
				      _ ->
					  noop
				  end,
				  ok
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

handle_call({client, join, Cookie, Facts}, _From, State) ->
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

		Criterion = get_criterion(),
		{ok, Releases} = find_matching_releases(Facts, Criterion),

		UpdatedClient = Client#client{joined=true, facts=Facts},
		mnesia:write(edist_controller_clients, UpdatedClient, write),
		edist_event_bus:notify(edist_agents, {join, Cookie, Facts}),
		{ok, Releases}
	end,
    try mnesia:transaction(F) of
	{atomic, {ok, Releases}} ->
	    {reply, {ok, [{releases, Releases}]}, State}
    catch
	_:Error ->
	    {reply, Error, State}
    end;

handle_call({client, subscribe_release, _Cookie, Rel}, {ClientPid, _}, State) ->
    try
	F = fun() ->
		    [#edist_release{config=Config}]
			= mnesia:read(edist_releases, Rel, read),
		    #edist_release_vsn{vsn=Vsn} = find_latest_active(Rel),

		    Props = [
			     {vsn, Vsn},
			     {config, Config}
			    ],
		    {ok, Props}
	    end,
	{atomic, {ok, Props}} = mnesia:transaction(F),

	Module = subscriber,
	StartFunc = {Module, start_link, [Rel, ClientPid]},
	
	{ok, Pid} = edist_controller_sup:start_child({erlang:now(),
						      StartFunc,
						      transient,
						      brutal_kill,
						      worker,
						      [Module]
						     }
						    ),
	{reply, {ok, Props, Pid}, State}
    catch
	_:Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({client, download_release, Cookie, Rel}, {ClientPid, _Tag}, State) ->
    try
	F = fun() ->
		    [Client]
			= mnesia:read(edist_controller_clients, Cookie, read),

		    Version = find_latest_active(Rel),
		    {Client, Version}
	    end,
	{atomic, {Client, Version}} = mnesia:transaction(F),

	Criterion = [{Element#edist_release_elem.criteria, Element}
		     || Element <- Version#edist_release_vsn.elements],

	Results = process_criterion(Client#client.facts, Criterion),

	% grab the first matching reply since the list is in prio order
	case lists:foldl(fun({nomatch, _}, nomatch) ->
				 nomatch;
			    ({match, Element}, nomatch) ->
				 Element;
			    (_, Element) ->
				 Element
			 end,
			 nomatch,
			 Results) of
	    nomatch ->
		throw("Matching criteria not found");
	    Element ->
		StartFunc = {release_output_device, start_link,
			     [Rel, Version, Element, ClientPid]},
		
		{ok, Dev} = edist_controller_sup:start_child({erlang:now(),
							      StartFunc,
							      transient,
							      brutal_kill,
							      worker,
							      [release_output_device]
							     }
							    ),

		{reply, {ok, Version#edist_release_vsn.vsn, Dev}, State}
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
			    mnesia:delete_object(Tab, Client, write),

			    edist_event_bus:notify(edist_agents,
					     {leave, Client#client.cookie})
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
		     active_vsn(Version)
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

active_vsn(Vsn) ->
    Vsn#edist_release_vsn.state =:= active.

exec_criteria(Facts, Criteria) ->
    % FIXME: We are JIT'ing the criteria, might want to 
    % precompile/cache somehow
    {ok, Fun} = util:compile_native(Criteria),
    Fun(Facts).

process_criterion(Facts, Criterion) ->
    % perform a parallel search for any elements with matching criteria   
    util:pmap(fun({Criteria, Cookie}) ->
		      case exec_criteria(Facts, Criteria) of
			  match -> {match, Cookie};
			  nomatch -> {nomatch, Cookie}
		      end
	      end,
	      Criterion).

get_criterion() ->
    Q = qlc:q([{G#edist_group.criteria, G}
	       || G <- mnesia:table(edist_groups)]),
    qlc:e(Q).

find_matching_releases(Facts, Criterion) ->
    % perform a parallel search against all registered groups
    % looking for any matches.  Any releases assigned to the
    % group are then assigned to the client

    Releases = lists:foldl(fun({match, Group}, Acc) ->
				   R = Group#edist_group.releases,
				   sets:union(Acc, sets:from_list(R));
			      (_, Acc) ->
				   Acc
			   end,
			   sets:new(),
			   process_criterion(Facts, Criterion)
			  ),
    {ok, sets:to_list(Releases)}.
