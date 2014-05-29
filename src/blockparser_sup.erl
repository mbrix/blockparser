-module(blockparser_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [pool_spec()],
    {ok, {{one_for_one, 1000, 3600}, ChildSpecs}}.

pool_spec() ->
    Name = block_pool,
    PoolArgs = [{name, {local, Name}},
                {worker_module, blockparser_worker},
                {size, erlang:system_info(logical_processors_available)*2},
                {max_overflow, 2}],
    poolboy:child_spec(Name, PoolArgs, []).
