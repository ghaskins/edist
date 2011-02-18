-module(install_iodevice).
-behavior(gen_server).

-export([start_link/2, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("release.hrl").

-define(CHARS_PER_REC, 4096).

-record(state, {name, vsn, position=0, buffer}).

close(IoDevice) ->
    gen_server:call(IoDevice, close).

start_link(Name, Vsn) ->
    gen_server:start_link(?MODULE, {Name, Vsn}, []).

init({Name, Vsn}) ->
    F = fun() ->
		Record = case mnesia:read(edist_releases, Name, write) of
			     [] -> #edist_release{name=Name};
			     [R] -> R
			 end,
		case dict:find(Vsn, Record#edist_release.versions) of
		    {ok, _} -> throw({exists, Name, Vsn});
		    error -> ok
		end,
		
		Version = #edist_release_vsn{
		  vsn=Vsn,
		  block_size=?CHARS_PER_REC,
		  total_size=initializing,
		  ref_count=0
		 },
		
		NewVersions = dict:store(Vsn, Version,
					 Record#edist_release.versions),
		NewRecord = Record#edist_release{versions=NewVersions},
		mnesia:write(edist_releases, NewRecord, write)
	end,
    {atomic, ok} = mnesia:transaction(F),

    {ok, #state{name=Name, vsn=Vsn, buffer= <<>>}}.

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

terminate(normal, #state{name=Name,vsn=Vsn} = State) ->
    % increment the reference count
    F = fun() ->
		[Record] = mnesia:read(edist_releases, Name, write),
		{ok, Version} = dict:find(Vsn, Record#edist_release.versions),
		Refs = Version#edist_release_vsn.ref_count,
		NewVersion = Version#edist_release_vsn{
			       total_size=State#state.position,
			       ref_count=Refs+1
			      },

		NewVersions = dict:store(Vsn, NewVersion,
					 Record#edist_release.versions),
		NewRecord = Record#edist_release{versions=NewVersions},
		mnesia:write(edist_releases, NewRecord, write)
	end,
    {atomic, ok} = mnesia:transaction(F),
    gen_event:notify({global, event_bus}, {release_installed, Name, Vsn}),
    ok;
terminate(_Reason, #state{name=Name, vsn=Vsn} = State) ->
    % issue a compensating transaction to remove all traces of this instance
    F = fun() ->
		Record = case mnesia:read(edist_releases, Name, write) of
			     [] -> #edist_release{name=Name};
			     [R] -> R
			 end,
		NewVersions = dict:erase(Vsn, Record#edist_release.versions),
		case dict:size(NewVersions) of
		    0 ->
			ok = mnesia:delete_object(edist_releases,
						  Record, write);
		    _ ->
			NewRecord = Record#edist_release{versions=NewVersions},
			ok = mnesia:write(edist_releases, NewRecord, write)
		end,
			
		Q = qlc:q([mnesia:delete_object(edist_release_blocks, R, write)
			   || R <- mnesia:table(edist_release_blocks),
			      R#edist_release_block.name == Name,
			      R#edist_release_block.vsn == Vsn
			  ]),
		qlc:e(Q),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
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
    Record = #edist_release_block{name=State#state.name,
				  vsn=State#state.vsn,
				  id=Row,
				  size=size(Data),
				  data=Data},
    F = fun() ->
		mnesia:write(edist_release_blocks, Record, write)
	end,
    {atomic, ok} = mnesia:transaction(F),
    State#state{buffer = <<>>}.
