%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(controller_sup).
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
	 [{'edist-controller',
	   {controller,start_link,[]},
	   permanent, 2000, worker,[controller]},
	  {'edist-event-bus',
	   {gen_event, start_link, [{global, edist_event_bus}]},
	   permanent, 5000, worker, dynamic}
	 ]
	}
    }.


