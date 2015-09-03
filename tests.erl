-module(tests).
-export ([odds_and_evens/1, cost/1, is_palindrome/1, my_tuple_to_list/1, my_time_func/1]).

odds_and_evens(L) ->
  odds_and_evens(L, [], []).

odds_and_evens([H|T], Odds, Evens) ->
  case (H rem 2) of
    1 ->
      odds_and_evens(T, [H|Odds], Evens);
    0 ->
      odds_and_evens(T, Odds, [H|Evens])
  end;
odds_and_evens([], Odds, Evens) ->
  {lists:reverse(Odds), lists:reverse(Evens)}.

cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.

is_equal([], []) ->
  true;
is_equal([H|T1], [H|T2]) ->
  is_equal(T1, T2);
is_equal(_, _) ->
  false.

is_palindrome([]) ->
  false;
is_palindrome(L) ->
  is_equal(L, lists:reverse(L)).

my_tuple_to_list(T) when is_tuple(T) ->
  my_tuple_to_list(T, 1, tuple_size(T)).

my_tuple_to_list(_, Pos, Size) when Pos > Size ->
  [];
my_tuple_to_list(T, Pos, Size) ->
  [element(Pos, T)|my_tuple_to_list(T, Pos + 1, Size)].


my_time_func(F) ->
  {_, _, Before} = erlang:now(),
  F(),
  {_, _, After} = erlang:now(),
  After - Before.