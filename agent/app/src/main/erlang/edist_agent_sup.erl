%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(edist_agent_sup).
-behaviour(supervisor).

-export([start_link/1, start_child/1, delete_child/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
        init/1
        ]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

delete_child(Id) ->
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id).
    
init([Path]) ->
    {ok,{{one_for_one,1,60},
	 [
	  {'edist-agent',
	   {edist_agent,start_link,[]},
	   permanent, 2000, worker,[edist_agent]},
	  {'edist-session',
	   {session_fsm,start_link,[Path]},
	   permanent, 2000, worker,[session_fsm]},
	  {'edist-connection',
	   {connection_fsm,start_link,[]},
	   permanent, 2000, worker,[connection_fsm]}
	 ]
	}
    }.


