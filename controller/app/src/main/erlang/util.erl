-module(util).
-export([open_table/2, replicas/0, atomic_query/1, compile_native/1, pmap/2]).

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

pmap(Fun, List) ->
    Parent = self(),
    Work = fun(Item) ->
		   try
		       Result = Fun(Item),
		       Parent ! {pmap, self(), Result}
		   catch
		       _:Error ->
			   Parent ! {pmap, self(), {error, Error}}
		   end
	   end,

    Pids = [spawn_link(fun() -> Work(Item) end)
	    || Item <- List],

    lists:map(fun(Pid) ->
		      receive
			  {pmap, Pid, {error, Error}} -> throw(Error);
			  {pmap, Pid, Result} -> Result
		      end
	      end,
	      Pids).



