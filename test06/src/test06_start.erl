%% -*- coding: utf-8 -*-
-module(test06_start).

-define(APPS, [
	sasl,
	os_mon,
	compiler,
	syntax_tools,
	goldrush,
	lager,
	crypto,
	ranch,
	cowlib,
	cowboy,
	test06
]).

-export([start/0, stop/0]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	{ok, CurrDir} = file:get_cwd(),
	io:fwrite("Current dir: ~p~n", [CurrDir]),
	% case mnesia:create_schema([node()]) of
	% 	ok -> io:fwrite("Mnesia schema created (First start)~n");
	% 	_ -> io:fwrite("Mnesia schema already exists (OK)~n")
	% end,
	ensure_started(?APPS).

stop() ->
	stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok ->
			ensure_started(Apps);
		{error, {already_started, App}} ->
			ensure_started(Apps)
	end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
	io:fwrite("Application [~p] stoped.~n", [App]),
	application:stop(App),
	stop_apps(Apps).
