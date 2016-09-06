%%% -*- coding: utf-8 -*-
-module(test07_pool_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% test07_pool_mgr callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-export([
	calc/2
]).

-define(SERVER, ?MODULE).

-record(state, {
	table :: ets:tid(),
	pools :: list(atom())
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

calc(PoolName, Val) ->
	gen_server:cast(?SERVER, {calc, PoolName, Val}).

%%%===================================================================
%%% tmsFiasPoolMgr callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) -> {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} | {stop, Reason :: term()} | ignore).
init([]) ->
	case init_pool() of
		{error, Reason} ->
			io:format("Plugins init error: ~p~n", [Reason]),
			{stop, plugin_init_error};
		State when is_record(State, state) ->
			start_pool(State),
			{ok, State}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_cast({calc, PoolName, Val}, State) ->
	cast_worker(State#state.table, PoolName, {calc, Val}),
	{noreply, State};

handle_cast(_Request, State) ->
	io:format("Undefined cast message: ~p~n", [_Request]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) ->
	{Module, _Node} = Object,
	case Module of
		tmsDbPoolSuper ->
			{noreply, State};
		_ ->
			{noreply, State}
	end;

handle_info(_Info, State) ->
	io:format("Undefined info message: ~p~n", [_Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a tmsFiasPoolMgr when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the tmsFiasPoolMgr terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> term()).
terminate(_Reason, _State) ->
 	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_pool() ->
	io:format("Workers init...~n"),
	Table = ets:new(test07_pools, [set, private]),
	case file:list_dir_all("./ebin") of
		{ok, FileNames} ->
			#state{table = Table, pools = find_workers(FileNames)};
		{error, Reason} ->
			{error, Reason}
	end.

find_workers(FileNames) ->
	lists:foldl(
		fun(F, AccIn) ->
			case filename:extension(F) of
				".beam" ->
					File = filename:basename(F, ".beam"),
					Module = list_to_atom(File),
					ModInfo = erlang:apply(Module, module_info, []),
					ModAttr = proplists:get_value(attributes, ModInfo),
					case proplists:get_value(plugin, ModAttr) of
						[PluginType] when PluginType == pool_worker -> 
							lists:append(AccIn, [Module]);
						_ -> 
							AccIn
					end;
				_ -> 
					AccIn
			end
		end, 
	[], FileNames).

start_pool(State) ->
	lists:foreach(
		fun(PoolName) -> 
			case application:get_application(self()) of
				{ok, AppName} ->
					case application:get_env(AppName, PoolName) of
						undefined ->
							io:format("Settings for '~p' poll undefined! Check '~p.config' file. Plugin skipped", [PoolName, AppName]);
						{ok, PoolParams} ->
							Size = proplists:get_value(pool_size, PoolParams),
							Processes = queue:from_list(add_children(PoolName, Size, [])),
							ets:insert(State#state.table, {PoolName, Processes})
					end;
				undefined ->
					error
			end
		end,
	State#state.pools).

child_spec(Module, Idx) ->
	Restart = permanent,
	Shutdown = 2000,
	ChildId = atom_to_list(Module) ++ integer_to_list(Idx),
	{ChildId, {Module, start_link, [Idx]}, Restart, Shutdown, worker, [Module]}.

add_children(_, 0, Acc) -> lists:flatten(Acc);
add_children(Module, Size, AccIn) ->
	Pid = case supervisor:start_child(test07_pool_super, child_spec(Module, Size)) of
		{ok, Child} -> Child;
		{ok, Child, _} -> Child;
		{error, _} -> []
	end,
	add_children(Module, Size - 1, lists:append(AccIn, [{Pid, 0}])).

next(Table, PoolName) ->
	case ets:match_object(Table, {PoolName, '$1'}) of
		[{_, Procs}|_] ->
			case queue:out(Procs) of
				{empty, _} ->
					pool_empty;
				{{value, {Pid, Count}}, P} ->
					ets:insert(Table, {PoolName, queue:in({Pid, Count+1}, P)}),
					Pid
			end;
		[] ->
			pool_empty
	end.

cast_worker(Table, PoolName, Message) ->
	case next(Table, PoolName) of
		pool_empty -> pool_empty;
		Pid -> gen_server:cast(Pid, Message)
	end.
