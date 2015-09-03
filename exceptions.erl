-module(exceptions).
-export([read/1]).

read(F) ->
  case file:read_file(F) of
    {ok, Bin} -> Bin;
    {error, Error} -> throw(Error)
  end.