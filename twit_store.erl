-module(twit_store).
-export([init/1, store/2, fetch/1]).

init(Num) ->
  {ok, File} = file:open("twit_store.dat", [raw, write, binary]),
  Size = Num * 140 * 8,
  file:pwrite(File, 0, <<0:Size>>),
  file:close(File).

store(Num, X) ->
  Filesize = filelib:file_size("twit_store.dat"),
  io:format("Size: ~p~n", [Filesize]),
  case Filesize >= Num * 140 of
    true ->
      {ok, File} = file:open("twit_store.dat", [raw, write, binary]),
      file:pwrite(File, (Num - 1) * 140, <<X:1120>>),
      file:close(File);
    false ->
      io:format("Exceeded tweet storage size")
  end.

fetch(Num) ->
  Filesize = filelib:file_size("twit_store.dat"),
  case Filesize >= Num * 140 of
    true ->
      {ok, File} = file:open("twit_store.dat", [raw, write, binary]),
      {ok, Data} = file:pread(File, (Num - 1) * 140, 140),
      file:close(File),
      Data;
    false ->
      io:format("Exceeded tweet storage size")
  end.