% Erlang Raw Block Parser - Matt Branton
-module(blockparser_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
		 handle_info/2, code_change/3, terminate/2, parse/2]).

-define(MAGICBYTE, 16#D9B4BEF9).

start_link([]) -> gen_server:start_link(?MODULE, [], []).
handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.
terminate(_Reason, _State) -> ok.

init([]) -> 
    process_flag(trap_exit, true),
	{ok, {}}.

parse(Pid, {Fname}) -> gen_server:cast(Pid, {parse, {Fname}}).

handle_call({parse, Fname}, _From, {}) ->
	case file:read_file(Fname) of
		{ok, Data} ->
			case file:open(Fname ++ ".csv", write) of
				{ok, File} ->
			    	extractLoop(File, Data),
					{reply, ok, {}};
				{error, Reason} ->
					{stop, error, Reason}
				end;
		{error, Reason} ->
			{stop, Reason, {Fname}}
	end.

handle_cast({parse, {Fname}}, State) ->
	case file:read_file(Fname) of
		{ok, Data} ->
			case file:open(Fname ++ ".csv", write) of
				{ok, File} ->
			    	extractLoop(File, Data),
					{noreply, State};
				{error, Reason} ->
					{stop, error, Reason}
				end;
		{error, Reason} ->
			{stop, Reason, State}
	end.

extractLoop(File, Data) ->
	case extract(Data) of
		{ok, Block, Next} ->
			  io:format(File, "~p ~n", [Block]),
			   extractLoop(File, Next);
		{scan, Next} ->
			<<_:8, Bin/binary>> = Next,
			extractLoop(File, Bin);
		ok ->
			ok
	end.

getVarInt(<< TXCount:8, BinRest/binary >>) when TXCount < 253 -> [TXCount, BinRest];
getVarInt(<< 253:8, TXCount:16/little, BinRest/binary >>) -> [TXCount, BinRest];
getVarInt(<< 254:8, TXCount:32/little, BinRest/binary >>) -> [TXCount, BinRest];
getVarInt(<< 255:8, TXCount:64/little, BinRest/binary >>) -> [TXCount, BinRest];
getVarInt(_) -> error.

getTxInputs(InputCount, Rest) -> getTxInputs(InputCount, Rest, []).
getTxInputs(0, Rest, Acc) -> [Acc, Rest];
getTxInputs(InputCount, Rest, Acc) ->
	<< Txhash:256/bitstring,
	   TxIndex:32/little,
	   BinRest/binary>> = Rest,
	   [ScriptLength, TxRest] = getVarInt(BinRest),
	   AdjustedScriptLength = ScriptLength*8,
	   << _Script:AdjustedScriptLength/bitstring, _SeqNum:32/little, Next/binary >> = TxRest,
	   getTxInputs(InputCount-1, Next, Acc++[Txhash, TxIndex]).

getTxOutputs(OutputCount, Rest) -> getTxOutputs(OutputCount, Rest, []).
getTxOutputs(0, Rest, Acc) -> [Acc, Rest];
getTxOutputs(OutputCount, Rest, Acc) ->
	<< Value:64/little,
	   BinRest/binary>> = Rest,
	   [ScriptLength, TxRest] = getVarInt(BinRest),
	   AdjustedScriptLength = ScriptLength*8,
	   << Script:AdjustedScriptLength/bitstring, Next/binary >> = TxRest,
	   getTxOutputs(OutputCount-1, Next, Acc++[Value, Script]).

getTransactions(TXCount, Tbin) -> getTransactions(TXCount, Tbin, []).
getTransactions(0, Tbin, Acc) -> [Acc, Tbin];
getTransactions(TXCount, Tbin, Acc) ->
	<< _TransactionVersion:32/little, Rest/binary >> = Tbin,
	[InputCount, Inputs] = getVarInt(Rest),
	[TxInputs, Rest2] = getTxInputs(InputCount, Inputs),
	[OutputCount, Outputs] = getVarInt(Rest2),
	[TxOutputs, Rest3] = getTxOutputs(OutputCount, Outputs),
    <<_TransactionLockTime:32/little, Next/binary>> = Rest3,
	getTransactions(TXCount-1, Next, Acc++[TxInputs ++ TxOutputs]).

extract(<< >>) -> ok;
extract(<<?MAGICBYTE:32/little, 
    HeaderLength:32/little,
    VersionNumber:32/little, 
    PreviousHash:256/bitstring, 
    MerkleRoot:256/bitstring, 
    TimeStamp:32/little, 
    TargetDifficulty:32/little, 
    Nonce:32/little,
    BinRest/binary>>) ->
   [TXCount, Tbin] = getVarInt(BinRest),
   [Tdata, _Rest] = getTransactions(TXCount, Tbin),
   {ok, {?MAGICBYTE, HeaderLength,
   	VersionNumber, PreviousHash,
    MerkleRoot, TimeStamp,
    TargetDifficulty, Nonce,
    TXCount, Tdata}, _Rest};
extract(<<R:8, _Bin/binary>>) when R > 0 ->
	io:format("Problem: ~w~n", [binary:bin_to_list(_Bin, {0, 10})]),
	{scan, _Bin};
extract(Data) -> {scan, Data}.
