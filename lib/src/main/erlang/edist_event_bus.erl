-module(edist_event_bus).
-export([subscribe/1, subscribe/2, notify/2, notify/3]).

-include_lib("stdlib/include/qlc.hrl").

-record(subscriber, {cookie}).
-record(options, {scope=global, cookie}).

subscribe(Options) ->
    subscribe(all, Options).

subscribe(EventType, RawOptions) ->
    Options = parse_options(RawOptions),
    Scope = case Options#options.scope of
		local -> l;
		global -> g
	    end,
    gproc:reg({p, Scope, {?MODULE, EventType}},
	      #subscriber{cookie=Options#options.cookie}).

notify(EventType, Msg) ->
    notify(EventType, Msg, []).

notify(EventType, Msg, RawOptions) ->
    Options = parse_options(RawOptions),
    Scope = Options#options.scope,

    Q = qlc:q([{P, S}
	       || {{p, '_', {?MODULE, E}}, P, S} <- gproc:table({Scope, props}),
		  E =:= EventType orelse E =:= all]),
    lists:foreach(fun({P, S}) ->
			  Header = [
				    {headerver, 1},
				    {from, self()},
				    {type, EventType},
				    {cookie, S#subscriber.cookie}
				   ],

			  P ! {?MODULE, Header, Msg}
		  end,
		  qlc:e(Q)).

parse_options(List) ->
    parse_options(List, #options{}).

parse_options([{scope, RawScope} | T], Options) ->
    Scope = case RawScope of
		local -> local;
		_ -> global
	    end,
    parse_options(T, Options#options{scope=Scope});
parse_options([{cookie, Cookie} | T], Options) ->
    parse_options(T, Options#options{cookie=Cookie});
parse_options([H | _T], _Options) ->
    throw({earg, H});
parse_options([], Options) ->
    Options.



