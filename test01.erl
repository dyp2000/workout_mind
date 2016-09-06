-module(test01).
-author("Dennis Y. Parygin").
-email("dyp2000@mail.ru").

-export([start/0]).

start() -> 
	io:format("Test #01 result: ~p~n", [ test_01(999) ]),
	io:format("Test #02 result: ~p~n", [ test_02(1, 1, 0)]),
	io:format("Test #03 result: ~p~n", [ test_03(600851475143) ]),
	io:format("Test #04 result: ~p~n", [ test_04() ]),
	io:format("Test #05 result: ~p~n", [ test_05() ]).

%%
%% Test 01
%%
test_01(Val) ->
	lists:sum([N || N <- lists:seq(1, Val), (N rem 3 =:= 0) or (N rem 5 =:= 0)]).

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
	R = [X || X <- lists:seq(2, trunc(math:sqrt(Val))), Val rem X =:= 0],
	case R of
		[] -> Val;
		[H|_] -> test_03(Val div H)
	end.

%%
%% Test 04
%%
list(L) ->
	lists:usort([A*B || A <- L, B <- L]).

is_pal(V) ->
	N = integer_to_list(V),
	N == lists:reverse(N).

test_04() -> 
	L = list(lists:seq(1, 999)),
	lists:max([X || X <- L, is_pal(X)]).

%%
%% Test 05
%%
g(X, 0) -> X;
g(X, Y) -> g(Y, X rem Y).

l(X, Y) -> (X * Y) div g(X, Y).

lm([X, Y| []]) -> l(X, Y);
lm([X, Y| T]) -> lm([l(X, Y) | T]).

test_05() ->
	lm(lists:seq(1,20)).
