-module(util).
-export([open_table/2, replicas/0, atomic_query/1]).

open_table(Name, TabDef) ->
    case mnesia:create_table(Name, TabDef) of
	{atomic, ok} -> ok;
	{aborted, {already_exists, Name}} -> ok;
	Error -> throw(Error)
    end.

replicas() ->
    [node()].

atomic_query(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

