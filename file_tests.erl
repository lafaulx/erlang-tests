-module(file_tests).
-export([need_to_recompile/1, compute_md5/1, compute_md5_large/1, find_duplicate_pics/1]).

need_to_recompile(Filename) ->
  {ok, {file_info, _, _, _, _, _, Ctime1, _, _, _, _, _, _, _}} = file:read_file_info([Filename|".erl"]),
  {ok, {file_info, _, _, _, _, _, Ctime2, _, _, _, _, _, _, _}} = file:read_file_info([Filename|".beam"]),
  Sec1 = calendar:datetime_to_gregorian_seconds(Ctime1),
  Sec2 = calendar:datetime_to_gregorian_seconds(Ctime2),
  Diff = Sec2 - Sec1,
  Diff < 0.

compute_md5(Filename) ->
  {ok, File} = file:read_file(Filename),
  erlang:md5(File).

compute_md5_large(Filename) ->
  case file:open(Filename, [read,binary,raw]) of
    {ok, File} -> compute_chunk_md5(File, 0, filelib:file_size(Filename), erlang:md5_init());
    {error, Error} -> io:format("Couldn't open file: ~p~n", [Error])
  end.

compute_chunk_md5(File, Start, Filesize, Ctx) ->
  End = Start + 128,
  if
    End < Filesize ->
      {ok, Chunk} = file:pread(File, Start, 128),
      compute_chunk_md5(File, End, Filesize, erlang:md5_update(Ctx, Chunk));
    End =:= Filesize ->
      {ok, Chunk} = file:pread(File, Start, 128),
      file:close(File),
      erlang:md5_final(erlang:md5_update(Ctx, Chunk));
    End > Filesize ->
      {ok, Chunk} = file:pread(File, Start, End - Filesize),
      file:close(File),
      erlang:md5_final(erlang:md5_update(Ctx, Chunk))
  end.

find_duplicate_pics(Dir) ->
  get_duplicate(lib_find:files(Dir, ".png", false, fun(Filename, AccIn) -> [{Filename, compute_md5_large(Filename)}|AccIn] end, []), []).

get_duplicate([], Acc) -> lists:flatten(Acc);
get_duplicate([{Filename, Md5}|T], Acc) ->
  Duplicate = lists:map(fun({F2, _}) -> {Filename, F2} end,lists:filter(fun({_, M}) -> M =:= Md5 end, T)),
  get_duplicate(T, [Duplicate|Acc]).

