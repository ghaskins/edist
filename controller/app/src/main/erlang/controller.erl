%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(controller).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("release.hrl").

-export([start_link/0, install_release/3]).

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
    Id = Name ++ "-" ++ Vsn,
    StartFunc = {release_input_device, start_link, [Name, Vsn]},

    case controller_sup:start_child({list_to_atom(Id),
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

handle_call({client, negotiate, ApiVsn, Args}, From, State) ->
    ApiVsn = api_version(),
    Cookie = erlang:now(),
    F = fun() ->
		Ref = erlang:monitor(process, From),
		Client = #client{cookie=Cookie, pid=From, ref=Ref},
		mnesia:write(edist_controller_clients, Client, write)
	end,
    {atomic, ok} = mnesia:transaction(F),
    {reply, {ok, ApiVsn, [], Cookie}, State};

handle_call({client, subscribe, Cookie, App}, From, State) ->
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
%%% Internal functions
%% --------------------------------------------------------------------

    
