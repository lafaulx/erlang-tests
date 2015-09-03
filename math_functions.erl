-module(math_functions).
-export([filter/2, even/1, odd/1, split_acc/1, split_filter/1]).

even(X) when is_integer(X) ->
  X rem 2 =:= 0.

odd(X) when is_integer(X) ->
  X rem 2 =:= 1.

filter(F, L) ->
  [X || X <- L, F(X)].

split_acc(L) when is_list(L) ->
  split_acc(L, [], []).

split_acc([], Even, Odd) ->
  {lists:reverse(Even), lists:reverse(Odd)};
split_acc([H|T], Even, Odd) ->
  case even(H) of
    true -> split_acc(T, [H|Even], Odd);
    false -> split_acc(T, Even, [H|Odd])
  end.

split_filter(L) when is_list(L) ->
  {filter(fun even/1, L), filter(fun odd/1, L)}.