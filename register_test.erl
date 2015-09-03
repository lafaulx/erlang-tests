-module(register_test).
-export([start/2]).

start(Atom, Fun) ->
  case (whereis(Atom)) of
    undefined ->
      register(Atom, spawn(Fun));
    _ ->
      {error, already_registered}
  end.