-module(release_input_device).
-behavior(gen_server).

-export([start_link/3, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("release.hrl").

-define(CHARS_PER_REC, 65536).

-record(state, {name, vsn, elem_id, position=0, buffer, sha_context}).

start_link(Name, Vsn, Criteria) ->
    gen_server:start_link(?MODULE, {Name, Vsn, Criteria}, []).

init({Name, Vsn, Criteria}) ->
    Id = erlang:now(),

    {ok, _} = util:compile_native(Criteria),

    F = fun() ->
		[Record] = mnesia:read(edist_releases, Name, write),
		{ok, Version} = dict:find(Vsn, Record#edist_release.versions),
		#edist_release_vsn{state=initializing} = Version,

		Element = #edist_release_elem{
		  elem_id=Id,
		  criteria=Criteria,
		  block_size=?CHARS_PER_REC,
		  total_size=initializing
		 },

		NewElements = Version#edist_release_vsn.elements ++ [Element],
		NewVersion = Version#edist_release_vsn{elements=NewElements},
		NewVersions = dict:store(Vsn, NewVersion,
					 Record#edist_release.versions),
		NewRecord = Record#edist_release{versions=NewVersions},
		mnesia:write(edist_releases, NewRecord, write)
	end,
    {atomic, ok} = mnesia:transaction(F),

    {ok, #state{name=Name, vsn=Vsn, elem_id=Id, buffer= <<>>,
		sha_context=crypto:sha_init()}}.

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

terminate(normal, State) ->
    #state{name=Name,vsn=Vsn,elem_id=Id,sha_context=ShaCtx} = State,

    % increment the reference count and save the final size
    UpdateElement = fun(Element) when Element#edist_release_elem.elem_id =:= Id ->
			    case Element#edist_release_elem.total_size of
				initializing ->
				    Sha = crypto:sha_final(ShaCtx),
				    Element#edist_release_elem{
				      total_size=State#state.position,
				      sha=Sha
				     }
			    end;
		       (Element) -> Element
		    end,

    UpdateVersion = fun(Version) ->
			    Refs = Version#edist_release_vsn.ref_count,
			    Elements = Version#edist_release_vsn.elements,
			    
			    NewElements = [UpdateElement(Element)
					   || Element <- Elements],
			    
			    Version#edist_release_vsn{
			      ref_count=Refs+1,
			      elements=NewElements
			     }
		    end,

    F = fun() ->
		edist_controller:update_version(Name, Vsn, UpdateVersion),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
    edist_event_bus:notify(edist_releases, {installed, Name, Vsn}),
    ok;
terminate(_Reason, #state{name=Name, vsn=Vsn} = _State) ->
    % issue a compensating transaction to remove all traces of this instance
    F = fun() ->
		edist_controller:rm_version(Name, Vsn)
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
    
    Updates = split_updates(Chars, R, C),
    NewState = lists:foldl(fun(E, Acc) ->
				   apply_update(E, Acc)
			   end,
			   State,
			   Updates),

    {ok, ok, NewState}.

split_updates([],_,_) ->
    [];
split_updates(Data, Row, Col) when size(Data) > (?CHARS_PER_REC - Col) ->
    LeftLen = ?CHARS_PER_REC - Col,
    RightLen = size(Data) - LeftLen,
    Left = binary:part(Data, {0, LeftLen}),
    Right = binary:part(Data, {LeftLen, RightLen}), 
    [ {Row, Col, Left} | split_updates(Right, Row + 1, 0) ];
split_updates(Data, Row, Col) ->
    [{Row, Col, Data}].

apply_update({Row, 0, Data}, State) when size(Data) =:= ?CHARS_PER_REC ->
    flush_buffer(Row, Data, advance_pos(Data, State));
apply_update({_Row, 0, Data}, State) ->
    (advance_pos(Data, State))#state{buffer=Data};
apply_update({Row, _Col, Data}, State) ->
    Buffer = State#state.buffer ++ Data,
    NewState = (advance_pos(Data, State))#state{buffer=Buffer},
    case NewState#state.position rem ?CHARS_PER_REC of
	0 -> flush_buffer(Row, Buffer, NewState);
	_ -> NewState
    end.

advance_pos(Data, State) ->
    P = State#state.position + size(Data),
    State#state{position=P}.    

flush_buffer(Row, Data, State) ->
    Record = #edist_release_block{id=erlang:now(),
				  name=State#state.name,
				  vsn=State#state.vsn,
				  elem_id=State#state.elem_id,
				  row=Row,
				  size=size(Data),
				  data=Data},
    F = fun() ->
		mnesia:write(edist_release_blocks, Record, write)
	end,
    {atomic, ok} = mnesia:transaction(F),
    Context = crypto:sha_update(State#state.sha_context, Data),
    State#state{buffer = <<>>, sha_context=Context}.
