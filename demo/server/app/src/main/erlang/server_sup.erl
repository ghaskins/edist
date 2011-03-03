%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(server_sup).
-behaviour(supervisor).

-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
        init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,{{one_for_all,0,1},
	 [{'edist-demo-server',
	   {server,start_link,[]},
	   permanent, 2000, worker,[server]}
	 ]
	}
    }.


