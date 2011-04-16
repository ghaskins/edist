%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(edist_controller_sup).
-behaviour(supervisor).

-export([start_link/1, start_child/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
        init/1
        ]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

start_link(Nodes) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Nodes]).

start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

init([Nodes]) ->
    {ok,{{one_for_all,0,1},
	 [
	  ?CHILD(edist_controller, worker, [Nodes]),
	  ?CHILD(event_logger, worker, [])
	 ]
	}
    }.


