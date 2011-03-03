-module(controller_api).
-export([api_version/0, negotiate/1, join/2, rejoin/3]).
-export([download_release/1, close_stream/1]).

api_version() -> 1.

-record(session, {pid, cookie}).

negotiate(Pid) ->
    ApiVsn = api_version(),
    Request = {client, negotiate, ApiVsn, []},
    {ok, ApiVsn, [], Cookie} = gen_server:call(Pid, Request),
    {ok, #session{pid=Pid, cookie=Cookie}}.

join(Session, Facts) ->
    gen_server:call(Session#session.pid,
		    {client, join, Session#session.cookie, Facts}).

rejoin(Session, Facts, App) ->
    gen_server:call(Session#session.pid,
		    {client, rejoin, Session#session.cookie, Facts, App}).

download_release(Session) ->
    gen_server:call(Session#session.pid,
		    {client, download_release, Session#session.cookie}).

close_stream(IoDevice) ->
    gen_server:call(IoDevice, close).

