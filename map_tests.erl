-module(map_tests).
-export([map_search_pred/2]).

map_search_pred(Map, Pred) when is_map(Map) ->
  map_search_pred(Map, maps:keys(Map), Pred).

map_search_pred(Map, [Key|T], Pred) ->
  Value = maps:get(Key, Map),

  case Pred(Key, Value) of
    true -> {Key, Value};
    false -> map_search_pred(Map, T, Pred)
  end.