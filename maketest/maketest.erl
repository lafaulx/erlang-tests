-module(maketest).
-export([sum/2, test/0]).

sum(X, Y) ->
  X + Y.

test() ->
  7 = sum(3, 4),
  0 = sum(-1, 1),
  io:format("Tests worked").