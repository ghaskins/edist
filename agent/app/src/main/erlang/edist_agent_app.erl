%% Author: ghaskins
-module(edist_agent_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    % FIXME
    StartArgs = [{contacts, ["contact1@linux-mp", "contact2@linux-mp"]}],

    Contacts = [ list_to_atom(Contact) ||
		   Contact <- proplists:get_value(contacts, StartArgs, [])
	       ],

    error_logger:info_msg("Starting with contacts: ~p (~p)~n",
			  [Contacts, StartArgs]),
    edist_agent_sup:start_link(Contacts).

stop(_State) ->
    ok.



