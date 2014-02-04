-module(facter).
-export([get_facts/0]).

get_facts() ->
    Facts = string:tokens(os_cmd:os_cmd("facter"), "\n"),

    F = fun(Fact) ->
		Pos = string:str(Fact, " => "),
		Key = string:substr(Fact, 1, Pos-1),
		Value = string:substr(Fact, Pos+4),
		{Key, Value}
    end,
    [ F(Fact) || Fact <- Facts].

