%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(edist_agent_sup).
-behaviour(supervisor).

-export([start_link/1, start_child/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
        init/1
        ]).

-define(SERVER, ?MODULE).

start_link(Contacts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Contacts]).

start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

init([Contacts]) ->

    F = fun(Contact) ->
		{Contact,
		 {contact_fsm, start_link, [Contact]},
		 transient,
		 brutal_kill,
		 worker,
		 [contact_fsm]}
	end,
    
    {ok,{{one_for_all,0,1},
	 [{'edist-agent',
	   {edist_agent,start_link,[]},
	   permanent, 2000, worker,[edist_agent]}
	 ] ++ [ F(Contact) || Contact <- Contacts ]
	}
    }.


