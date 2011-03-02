-module(release_output_device).
-behavior(gen_server).

-export([start_link/3, init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include_lib("release.hrl").

-record(state, {name, vsn, elem_id, size, blksize, position=0}).

start_link(Name, Version, ClientPid) ->
    gen_server:start_link(?MODULE, {Name, Version, ClientPid}, []).

init({Name, Version, ClientPid}) ->
    erlang:monitor(process, ClientPid),
    % FIXME: just grab the first element
    #edist_release_vsn{vsn=Vsn, elements=[Element | _]} = Version,
    #edist_release_elem{elem_id=Id,
			block_size=BlkSize,
			total_size=Size} = Element,
    F = fun() ->
		controller:inc_version(Name, Vsn),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
    {ok, #state{name=Name, vsn=Vsn, elem_id=Id, size=Size, blksize=BlkSize}}.

handle_call(close, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, enotsup}, State}. 

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _Info}, State) ->
    {stop, normal, ok, State};
handle_info({io_request, From, ReplyAs, Request}, State) ->
    {_Tag, Reply, NewState} = io_request(Request, State),
    From ! {io_reply, ReplyAs, Reply},
    {noreply, NewState}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{name=Name,vsn=Vsn}) ->
    F = fun() ->
		Version = controller:dec_version(Name, Vsn),
		if
		    Version#edist_release_vsn.ref_count =:= 0 ->
			controller:rm_version(Name, Vsn);
		    true ->
			ok
		end
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

io_request({requests, Reqs}, State) ->
    multi_request(Reqs, {ok, ok, State});
io_request({get_chars, '', Count}, State) ->
    get_chars(Count, State);
io_request(Request, State) ->
    error_logger:warning_msg("Unknown IO request ~p~n", [Request]),
    {error, {error, request}, State}.

multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, io_request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], Result) ->
    Result.

get_chars(_Count, #state{size=Size, position=P} = State)
  when P >= Size ->
    {error, eof, State};
get_chars(Count, #state{size=Size, position=P} = State)
  when P+Count > Size ->
    get_chars(Size-P, State);
get_chars(Count, State) ->
    #state{name=Name, vsn=Vsn, elem_id=Id, blksize=BlkSize, position=P} = State,
    StartRow = P div BlkSize,
    StartCol = P rem BlkSize,
    
    Blocks = split_request(StartRow, StartCol, Count, BlkSize),

    F = fun() ->
		lists:foldl(fun({Row, Col, N}, Current) ->
				    Next = process_request(Name, Vsn, Id, Row, Col, N, BlkSize),
				    <<Current/binary, Next/binary>> 
			    end,
			    <<>>,
			    Blocks)
	end,
    {atomic, Data} = mnesia:transaction(F),
    {ok, Data, State#state{position=P+Count}}.

split_request(_,_,0,_) ->
    [];
split_request(Row, Col, Count, BlkSize) when Count > (BlkSize - Col) ->
    Left = BlkSize - Col,
    Right = Count - Left,
    [ {Row, Col, Left} | split_request(Row + 1, 0, Right, BlkSize) ];
split_request(Row, Col, Count, _BlkSize) ->
    [{Row, Col, Count}].

process_request(Name, Vsn, Id, Row, 0, Count, BlkSize) when Count =:= BlkSize ->
    find_block(Name, Vsn, Id, Row, 0, Count);
process_request(Name, Vsn, Id, Row, Col, Count, _BlkSize) ->
    Data = find_block(Name, Vsn, Id, Row, Col, Count),
    binary:part(Data, {Col, Count}).

find_block(Name, Vsn, Id, Row, Col, Count) ->   
    Q = qlc:q([R || R <- mnesia:table(edist_release_blocks),
		    R#edist_release_block.name =:= Name,
		    R#edist_release_block.vsn =:= Vsn,
		    R#edist_release_block.elem_id =:= Id,
		    R#edist_release_block.row =:= Row
	      ]),
    [Block] = qlc:e(Q),
    if
	Col+Count > Block#edist_release_block.size ->
	    throw({"Block too small", Col+Count, Block}); 
	true ->
	    Block#edist_release_block.data
    end.

