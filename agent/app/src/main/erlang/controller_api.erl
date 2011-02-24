-module(controller_api).
-export([api_version/0, negotiate/1, subscribe/2]).

api_version() -> 1.

-record(session, {pid, cookie}).

negotiate(Pid) ->
    ApiVsn = api_version(),
    Request = {client, negotiate, ApiVsn, []},
    {ok, ApiVsn, [], Cookie} = gen_server:call(Pid, Request),
    {ok, #session{pid=Pid, cookie=Cookie}}.

subscribe(Session, App) ->
    gen_server:call(Session#session.pid,
		    {client, subscribe, Session#session.cookie, App}).

open_latest(Session) ->
    gen_server:call(Session#session.pid,
		    {client, open_latest, Session#session.cookie}).

close_device(IoDevice) ->
    gen_server:call(IoDevice, close).

