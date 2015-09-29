-module(socket_tests).
-export([nano_get_url/1, nano_client_eval_tcp/3, start_nano_server_tcp/0, nano_client_eval_udp/3, start_nano_server_udp/0]).
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

nano_client_eval_tcp(Mod, Func, Args) ->
  {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 4}]),
  ok = gen_tcp:send(Socket, term_to_binary({Mod, Func, Args})),
  receive
    {tcp,Socket,Bin} ->
      io:format("Client received binary = ~p~n",[Bin]),
      Val = binary_to_term(Bin),
      io:format("Client result = ~p~n",[Val]),
      gen_tcp:close(Socket)
  end.

start_nano_server_tcp() ->
  {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
  spawn(fun() -> par_loop_tcp(Listen) end).

par_loop_tcp(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> par_loop_tcp(Listen) end),
  loop_tcp(Socket).

loop_tcp(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      io:format("Server received binary = ~p~n",[Bin]),
      {Mod, Func, Arg} = binary_to_term(Bin),
      io:format("Server (unpacked)  ~p ~p ~p~n",[Mod, Func, Arg]),
      Reply = apply(Mod, Func, Arg),
      io:format("Server replying = ~p~n",[Reply]),
      gen_tcp:send(Socket, term_to_binary(Reply)),
      loop_tcp(Socket);
    {tcp_closed, Socket} ->
      io:format("Server socket closed~n")
  end.

nano_client_eval_udp(Mod, Func, Args) ->
  {ok, Socket} = gen_udp:open(0, [binary]),
  io:format("client opened socket=~p~n",[Socket]),
  ok = gen_udp:send(Socket, "localhost", 4000, term_to_binary({Mod, Func, Args})),
  Value =
    receive {udp, Socket, _, _, Bin} = Msg ->
      io:format("client received:~p~n",[Msg]),
      binary_to_term(Bin)
    after 2000 ->
      0
    end,
  gen_udp:close(Socket),
  Value.

start_nano_server_udp() ->
  {ok, Socket} = gen_udp:open(4000, [binary]),
  loop_udp(Socket).

loop_udp(Socket) ->
  receive
    {udp, Socket, Host, Port, Bin} = Msg ->
      io:format("Server received binary = ~p~n",[Msg]),
      {Mod, Func, Arg} = binary_to_term(Bin),
      io:format("Server (unpacked)  ~p ~p ~p~n",[Mod, Func, Arg]),
      Reply = apply(Mod, Func, Arg),
      io:format("Server replying = ~p~n",[Reply]),
      gen_udp:send(Socket, Host, Port, term_to_binary(Reply)),
      loop_udp(Socket)
  end.