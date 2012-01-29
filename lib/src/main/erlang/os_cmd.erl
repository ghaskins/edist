-module(os_cmd).
-export([os_cmd_format/2, os_cmd_format/3, os_cmd/1, os_cmd/2]).

format(String, Args) ->
    lists:flatten(io_lib:format(String, Args)).

os_cmd_format(CmdFormat, Params) ->
    os_cmd_format(CmdFormat, Params, false).

os_cmd_format(CmdFormat, Params, Verbose) ->
    Cmd = format(CmdFormat, Params),
    os_cmd(Cmd, Verbose).

os_cmd(Cmd) ->
    os_cmd(Cmd, 0, false).

os_cmd(Cmd, Verbose) ->
    os_cmd(Cmd, 0, Verbose).

os_cmd(Cmd, ExpectedStatus, Verbose) ->
    case Verbose of
	true -> io:format("[INFO] Running \"~s\"~n", [Cmd]);
	false -> nop
    end,

    Port = erlang:open_port({spawn, Cmd}, [use_stdio, exit_status]),
    case cmd_receive(Port, "", Verbose) of
        {ok, ExpectedStatus, Data} ->
            Data;
        {ok, UnexpectedStatus, Data} ->
            throw({badstatus, [{status, UnexpectedStatus},
                               {cmd, Cmd},
                               {data, Data}]})
    end.

cmd_receive(Port, Acc, Verbose) ->
    receive
        {Port, {data, Data}} ->
	    case Verbose of
		true -> io:format("~s", [Data]);
		false -> nop
	    end,
            cmd_receive(Port, string:concat(Acc, Data), Verbose);
        {Port, {exit_status, Status}} ->
            {ok, Status, Acc}
    end.
