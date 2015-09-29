-module(socket_tests).
-export([nano_get_url/1]).
-import(lists, [reverse/1]).

nano_get_url({http, _Userinfo, Host, _Port, Path, Query}) ->
  {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}, {active, true}]),
  ok = gen_tcp:send(Socket, "GET " ++ Path ++ Query ++ " HTTP/1.0\r\n\r\n"),
  receive_data(Socket, []);
nano_get_url(Host) ->
  {ok, URL} = http_uri:parse(Host),
  nano_get_url(URL).

receive_data(Socket, SoFar) ->
  receive
    {tcp,Socket,Bin} ->
      receive_data(Socket, [Bin|SoFar]);
    {tcp_closed,Socket} ->
      Response = list_to_binary(reverse(SoFar)),
      case parse_header(Response) of
        [{http_response, _Version, 200, _String}|_Headers] ->
          Response;
        [{http_response, _Version, 301, _String}|Headers] ->
          nano_get_url(get_redirect_location(Headers));
        [{http_response, _Version, 302, _String}|Headers] ->
          nano_get_url(get_redirect_location(Headers))
      end
  end.

get_redirect_location(Headers) ->
  {http_header, _, 'Location', _, LocationBin} = lists:keyfind('Location', 3, Headers),
  erlang:binary_to_list(LocationBin).

parse_header(Bin) ->
  parse_header(erlang:decode_packet(http_bin, Bin, []), []).

parse_header({ok, http_eoh, _}, Acc) ->
  reverse(Acc);
parse_header({ok, Data, Rest}, Acc) ->
  parse_header(erlang:decode_packet(httph_bin, Rest, []), [Data|Acc]).