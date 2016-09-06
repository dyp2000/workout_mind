%%% -*- coding: utf-8 -*-
-module(test07_super).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) -> 
	{ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(), MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},	[ChildSpec :: supervisor:child_spec()]}} |
	ignore | {error, Reason :: term()}).
init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxSecondsBetweenRestarts = 3600,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
	Restart = permanent,
	Shutdown = 2000,
	PoolSuperSpec = {test07_pool_super, {test07_pool_super, start_link, []}, Restart, Shutdown, supervisor, [test07_pool_super]},
	PoolMgrSpec = {test07_pool_mgr, {test07_pool_mgr, start_link, []}, Restart, Shutdown, worker, [test07_pool_mgr]},
	MainSpec = {test07_main, {test07_main, start_link, []}, Restart, Shutdown, worker, [test07_main]},
 	{ok, {SupFlags, [PoolSuperSpec, PoolMgrSpec, MainSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
