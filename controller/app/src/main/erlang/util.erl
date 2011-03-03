-module(util).
-export([open_table/2, replicas/0, atomic_query/1, compile_native/1]).

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

compile_native(matchall) ->
    compile_native("fun(Facts) -> match end.");
compile_native(ExpStr) ->
    % compile the expression into a fun, or die tryin'
    {ok, Tokens, _} = erl_scan:string(ExpStr),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    {value, Exp, _} = erl_eval:expr(Form, erl_eval:new_bindings()),
    {ok, Exp}.


