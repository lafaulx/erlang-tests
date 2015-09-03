-module(misc).
-import(code, [all_loaded/0]).
-import(lists, [last/1, keysort/2, flatten/1, filter/2]).
-export([find_max_fn_module/0, find_most_common_fn_name/0, get_unambiguous_fn_list/0]).

find_max_fn_module() ->
  last(keysort(2, get_modules_fn_num_list())).

find_most_common_fn_name() ->
  last(keysort(2, get_fn_frequencies(get_modules_fn_list()))).

get_modules_fn_list() ->
  flatten([apply(Mod, module_info, [functions]) || {Mod, _File} <- all_loaded()]).

get_modules_fn_num_list() ->
  [{Mod, length(apply(Mod, module_info, [functions]))} || {Mod, _File} <- all_loaded()].

get_unambiguous_fn_list() ->
  filter(fun({_, Freq}) -> Freq =:= 1 end, get_fn_frequencies(get_modules_fn_list())).

get_fn_frequencies(L) ->
  get_fn_frequencies(L, dict:new()).

get_fn_frequencies([], D) -> dict:to_list(D);
get_fn_frequencies([{Key, _}|T], D) ->
  get_fn_frequencies(T, dict:update_counter(Key, 1, D)).