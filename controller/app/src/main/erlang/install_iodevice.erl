-module(install_iodevice).
-behavior(gen_server).

-export([start_link/3, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("release.hrl").

-define(CHARS_PER_REC, 4096).

-record(state, {name, vsn, size, position=0, buffer}).

close(IoDevice) ->
    gen_server:call(IoDevice, close).

start_link(Name, Vsn, Size) ->
    gen_server:start_link(?MODULE, {Name, Vsn, Size}, []).

init({Name, Vsn, Size}) ->
    Buffer = <<>>,
    {ok, #state{name=Name, vsn=Vsn, size=Size, buffer=Buffer}}.

handle_call(close, _From, #state{buffer=Buffer} = State) when size(Buffer) > 0 ->
    P = State#state.position,
    R = P div ?CHARS_PER_REC,
    NewState = flush_buffer(R, Buffer, State),
    {stop, normal, ok, NewState};
handle_call(close, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, enotsup}, State}. 

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({io_request, From, ReplyAs, Request}, State) ->
    {_Tag, Reply, NewState} = io_request(Request, State),
    From ! {io_reply, ReplyAs, Reply},
    {noreply, NewState}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

io_request({requests, Reqs}, State) ->
    multi_request(Reqs, {ok, ok, State});
io_request({put_chars, Chars}, State) when is_binary(Chars) ->
    put_chars(Chars, State);
io_request({put_chars, M, F, A}, State) ->
    try
	io_request({put_chars, apply(M, F, A)}, State)
    catch
	_:_ ->
	    {error, {error, F}, State}
    end;
io_request(Request, State) ->
    error_logger:warning_msg("Unknown IO request ~p~n", [Request]),
    {error, {error, request}, State}.

multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, io_request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], Result) ->
    Result.

put_chars(Chars, #state{position = P} = State) ->
    R = P div ?CHARS_PER_REC,
    C = P rem ?CHARS_PER_REC,
    
    NewState = lists:foldl(fun(E, Acc) ->
				   apply_update(E, Acc)
			   end,
			   State,
			   split_updates(Chars, R, C) ),

    {ok, ok, NewState}.

split_updates([],_,_) ->
    [];
split_updates(Data, Row, Col) when size(Data) > (?CHARS_PER_REC - Col)->
    {This,Left} = binary:part(Data, ?CHARS_PER_REC - Col),
    [ {Row, Col, This} | split_updates(Left, Row + 1, 0) ];
split_updates(Data, Row, Col) ->
    [{Row, Col, Data}].

apply_update({Row, 0, Data}, State) when size(Data) =:= ?CHARS_PER_REC ->
    flush_buffer(Row, Data, State);
apply_update({_Row, 0, Data}, State) ->
    P = State#state.position + size(Data),
    State#state{buffer=Data, position=P};
apply_update({Row, _Col, Data}, State) ->
    Buffer = State#state.buffer ++ Data,
    P = State#state.position + size(Data),
    NewState = State#state{buffer=Buffer, position=P},
    case P rem ?CHARS_PER_REC of
	0 -> flush_buffer(Row, Buffer, NewState);
	_ -> NewState
    end.

flush_buffer(Row, Data, State) ->
    Record = #edist_release_data{name=State#state.name,
				 vsn=State#state.vsn,
				 block_id=Row,
				 block_size=size(Data),
				 data=Data},
    F = fun() ->
		mnesia:write(edist_releases_data, Record, write)
	end,
    {atomic, ok} = mnesia:transaction(F),
    State.
