%%% -*- coding: utf-8 -*-
-module(test06).

-behaviour(application).

%% Application callbacks
-export([
	start/2,
	stop/1,
	custom_404_hook/4
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()}, StartArgs :: term()) ->
	{ok, pid()} |
	{ok, pid(), State :: term()} |
	{error, Reason :: term()}).

start(_StartType, _StartArgs) ->
	init_cowboy(),
	test06_super:start_link().

init_cowboy() ->
	%% Config and Start COWBOY
	RoutersWeb = routes_web(),
	DispatchWeb = cowboy_router:compile(RoutersWeb),
	HttpPort = port(http_port),
	TransOptsWeb = [{port, HttpPort}],
	ProtoOptsWeb = [
		{env, [{dispatch, DispatchWeb}]},
		{onresponse, fun ?MODULE:custom_404_hook/4}
	],
	WebRes = cowboy:start_http(web, 32, TransOptsWeb, ProtoOptsWeb),
	io:fwrite("Web Server started: ~p~n", [WebRes]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
	halt(),
	stop.

custom_404_hook(404, Headers, <<>>, Req) ->
    {ok, Body} = file:read_file("./html/error-page.html"),
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers, {<<"content-length">>, integer_to_list(byte_size(Body))}),
    {ok, Req2} = cowboy_req:reply(404, Headers2, Body, Req),
    Req2;
custom_404_hook(_, _, _, Req) ->
    Req.

%%%===================================================================
%%% Internal functions
%%%===================================================================
routes_web() ->
	[
		{'_', 
			[
				{"/capture", test06_handler, []},
				{"/", cowboy_static, {file, "./html/index.html"}}
			]
		}
	].

port(http_port) -> 
	application:get_env(test06, http_port, 9080).

