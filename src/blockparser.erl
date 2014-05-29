-module(blockparser).
-behaviour(application).
-export([start/2, stop/1]).
% Public API
-export([parse/1, parse_dir_seq/0, parse_list_seq/1,
         parse_dir/0, parse_list_parallel/1]).

start(_StartType, _StartArgs) ->
	blockparser_sup:start_link().

stop(_State) ->
	blockparser_sup:stop().

% Application entry points
parse(Fname) ->
	poolboy:transaction(block_pool, fun(Worker) ->
		gen_server:call(Worker, {parse, Fname}, infinity) end).

parse_dir_seq() -> parse_list_seq(filelib:wildcard("*.dat")).
parse_list_seq([]) -> [];
parse_list_seq(Flist) ->
	[H|T] = Flist,
    parse(H),
    parse_list_seq(T).

% Lets split the work load and then queue to spawned processes
% Yes, I realize this is relatively terrible.
parse_dir() -> parse_list_parallel(filelib:wildcard("*.dat")).
parse_list_parallel(Flist) ->
	SplitList = n_length_chunks_fast(Flist, 
			erlang:trunc(length(Flist)/erlang:system_info(logical_processors_available))),
	SplitList = n_length_chunks_fast(Flist, 1),
    spawn_worker(SplitList).

spawn_worker([]) -> [];
spawn_worker(List) ->
	[H|T] = List,
	_Pid = spawn(fun() -> parse_list_seq(H) end),
    spawn_worker(T).
	
% compliments stack overflow -> http://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang user chops.
n_length_chunks_fast(List,Len) ->
  n_length_chunks_fast(lists:reverse(List),[],0,Len).
n_length_chunks_fast([],Acc,_,_) -> Acc;
n_length_chunks_fast([H|T],Acc,Pos,Max) when Pos==Max ->
    n_length_chunks_fast(T,[[H] | Acc],1,Max);
n_length_chunks_fast([H|T],[HAcc | TAcc],Pos,Max) ->
    n_length_chunks_fast(T,[[H | HAcc] | TAcc],Pos+1,Max);
n_length_chunks_fast([H|T],[],Pos,Max) ->
    n_length_chunks_fast(T,[[H]],Pos+1,Max).
%%% end %%%
