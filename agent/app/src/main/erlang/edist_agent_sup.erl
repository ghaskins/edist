%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(edist_agent_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
        init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

init([]) ->
    {ok,{{one_for_all,0,1},
	 [{'edist-agent',
	   {edist_agent,start_link,[]},
	   permanent, 2000, worker,[edist_agent]}
	 ]
	}
    }.


