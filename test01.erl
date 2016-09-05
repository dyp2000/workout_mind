-module(test01).
-author("Dennis Y. Parygin").
-email("dyp2000@mail.ru").

-export([start/0]).

start() -> 
	io:format("Test #01 result: ~p~n", [ test_01(999, 0) ]),
	io:format("Test #02 result: ~p~n", [ test_02(1, 1, 0)]).

%%
%% Test 01
%%
test_01(0, Sum) -> Sum;
test_01(Val, Sum) ->
	S = case Val of
		V when V rem 3 =:= 0 -> Sum + V;
		V when V rem 5 =:= 0 -> Sum + V;
		_ -> Sum
	end,
	test_01(Val - 1, S).


%%
%% Test 02
%%
test_02(F1, F2, Sum) when F2 =< 4000000 ->
	F3 = F1 + F2,
	S = case F3 of
		V when V rem 2 =:= 0 -> Sum + V;
		_ -> Sum
	end,
	test_02(F2, F3, S);
test_02(_, _, Sum) -> Sum.

%%
%% Test 03
%%
test_03(Val) ->
	case Res of
	 	R when 600851475143 rem Val =:= 0 -> R
	 	_ -> Res
	 end 
