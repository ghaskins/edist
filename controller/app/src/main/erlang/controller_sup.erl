%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% -------------------------------------------------------------------
-module(controller_sup).
-behaviour(supervisor).

-export([start_link/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link(?MODULE, []).


init([]) ->
    Child = {'edist-controller',{controller,start_link,[]},
	      permanent,2000,worker,[controller]},
    {ok,{{one_for_all,0,1}, [Child]}}.


