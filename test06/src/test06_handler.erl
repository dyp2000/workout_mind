%% -*- coding: utf-8 -*-
-module(test06_handler).
-compile([{parse_transform, lager_transform}]).

-export([
	init/3,
	handle/2,
	terminate/3
]).

-include_lib("xmerl/include/xmerl.hrl").

-define(utf8(Chars), unicode:characters_to_list(Chars)).
-define(chrBin(Chars), unicode:characters_to_binary(Chars)).

init(_Type, Req, _State) ->
 	{ok, Req, no_state}.

handle(Req, State) ->
	case cowboy_req:body(Req) of
		{ok, Bin, Req2} ->
			Attrs = capture(Bin),
			case accept(Attrs) of
				true ->
					write_csv(Attrs),
					{ok, Req3} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"ok">>, Req2),
    				{ok, Req3, State};
				false ->
					{ok, Req3} = cowboy_req:reply(406, [{<<"content-type">>, <<"text/plain">>}], <<"not acceptible">>, Req2),
    				{ok, Req3, State}
			end;
		{error, Reason} ->
			lager:error("Error: ~p", [Reason]),
			{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"error">>, Req),
    		{ok, Req2, State}
	end.

terminate(_Reason, _Req, _State) ->
	ok.

%%
%%  internal functions
%%

write_csv(Attrs) -> 
	write_csv(Attrs, []).

write_csv([H|T], Acc) -> 
	case proplists:get_value(baseAttrId, H) of
		"PROD_COVER_GTIN" ->
			write_csv(T, lists:append(Acc, [{1, proplists:get_value(value, H)}]));
		"PROD_NAME" ->
			write_csv(T, lists:append(Acc, [{2, proplists:get_value(value, H)}]));
		"PROD_DESC" ->
			write_csv(T, lists:append(Acc, [{3, proplists:get_value(value, H)}]));
		"BRAND_OWNER_NAME" ->
			write_csv(T, lists:append(Acc, [{4, proplists:get_value(value, H)}]));
		_ ->
			write_csv(T, Acc)
	end;
write_csv([], Acc) ->
	% lager:debug("Acc: ~p", [Acc]),
	file:delete("./result.csv"),
	{ok, IoDev} = file:open("./result.csv", [append, binary]),
	file:write(IoDev, "GTIN,NAME,DESC,COMPANY\n"),
	Data = iolist_to_binary([iolist_to_binary([binary:replace(?chrBin(Val), <<"\"">>, <<"&quot;">>, [global]), ","]) || {_, Val} <- lists:sort(Acc)]),
	Sz = byte_size(Data) - 1,
	<<Bin:Sz/binary, ",">> = Data,
	file:write(IoDev, Bin), 
	file:write(IoDev, "\n"),
	file:close(IoDev).

accept(Attrs) -> accept(Attrs, [{gtin, false}, {name, false}]).

accept([H|T], Acc) ->
	case proplists:get_value(baseAttrId, H) of
		"PROD_COVER_GTIN" -> accept(T, lists:keyreplace(gtin, 1, Acc, {gtin, true}));
		"PROD_NAME" -> accept(T, lists:keyreplace(name, 1, Acc, {name, true}));
		_ -> accept(T, Acc)
	end;
accept([], Acc) -> 
	proplists:get_value(gtin, Acc) and proplists:get_value(name, Acc).

attr([H|T], Acc) ->
	Acc1 = lists:append(Acc, [{H#xmlAttribute.name, H#xmlAttribute.value}]),
	attr(T, Acc1);
attr([], Acc) ->
	Acc.

attr(Attr) when is_binary(Attr) ->
	Elem = iolist_to_binary([<<"<value">>, Attr, <<"/>">>]),
	{Xml, _} = xmerl_scan:string(binary_to_list(Elem)),
	attr(Xml#xmlElement.attributes, []).

capture(<<"<value", Bin/binary>>, Acc) ->
	[Attr, Rest] = binary:split(Bin, <<"/>">>),
	Acc1 = lists:append(Acc, [attr(Attr)]),
	capture(Rest, Acc1);
capture(<<>>, Attrs) ->
	Attrs.

capture(<<"<value", Bin/binary>>) ->
	[Attr, Rest] = binary:split(Bin, <<"/>">>),
	capture(Rest, [attr(Attr)]);

capture(<<"<BaseAttributeValues>", Bin/binary>>) ->
	[Data, _] = binary:split(Bin, <<"</BaseAttributeValues>">>),
	capture(Data);
capture(<<"<", Bin/binary>>) ->
	[_, Rest] = binary:split(Bin, <<">">>),
	capture(Rest);
capture(<<_:1/binary, Bin/binary>>) ->
	capture(Bin);
capture(<<>>) ->
	eod.