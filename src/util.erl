-module(util).
-export([mod/2, clamp/3, test/0]).

mod(X, Y) ->
	(X rem Y + Y) rem Y.

clamp(Min, Val, Max) ->
	if
		Val < Min -> Min;
		Val > Max -> Max;
		true      -> Val
	end.

test() ->
	1 = mod(5, 2),
	1 = mod(1, 5),
	4 = mod(-1, 5),
	0 = clamp(0, -50, 100),
	50 = clamp(0, 50, 100),
	100 = clamp(0, 150, 100).
