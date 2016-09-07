%%% -*- coding: utf-8 -*-
-module(test08).

%% API
-export([validate/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec validate({gtin, Value::iolist()|binary()}) -> ok | {error, term()}.

validate({gtin, Value}) when is_list(Value) ->
	validate({gtin, iolist_to_binary(Value)});

validate({gtin, Value}) when is_binary(Value) ->
	case check_size(Value) of
		ok ->
			case check_int(Value) of
				ok -> 
					<<V:13/binary,_/binary>> = Value,
					Sum = sum(V, 1, 0),
					% io:format("Calc Sum: ~p~n", [Sum]),
					check(Value, Sum);
				error ->
					{error, gtin14_bad_format}
			end;
		error -> 
			{error, size_error}
	end;

validate(_) ->
	{error, value_error}.

%%%===================================================================
%%% internal functions
%%%===================================================================

check(<<_T:13/binary, CS:1/binary>>, Sum) -> 
	S = binary_to_integer(CS),
	case S == Sum of
		true -> ok;
		false -> {error, gtin14_not_valid}
	end.

sum(<<>>, _Idx, Sum) -> 
	% io:format("Sum: ~p~n", [Sum]),
	chksum(Sum);
sum(<<B:1/binary, Tail/binary>>, Idx, Sum) ->
	S = case Idx of
		I when I rem 2 =/= 0 -> binary_to_integer(B) * 3;
		I when I rem 2 =:= 0 -> binary_to_integer(B)
	end,
	sum(Tail, Idx+1, Sum+S).

chksum(Val) ->
	case Val of 
		V when V rem 10 == 0 ->
			0;
		V when V rem 10 >= 5 ->	
			round(V/10) * 10 - Val;
		V when V rem 10 < 5 ->
			(round(V/10) * 10 + 10) - Val
	end.

check_int(<<>>) -> ok;
check_int(<<V:1/binary, Tail/binary>>) ->
	B = binary:at(V, 0),
	case B of
		X when X >= 48, X =< 57 ->
			check_int(Tail);
		_ ->
			error
	end.

check_size(Value) ->
	case byte_size(Value) of
		14 -> ok;
		_ -> error
	end.