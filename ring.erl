-module(ring).
-export([start/2, create_process/1, create_tail_process/1, create_head_process/2]).

start(_, 0) ->
  void;
start(N, M) when N > 1 ->
  io:format("Creating ring transmission\n"),
  setup_ring(N, M, []),
  start(N, M-1).

setup_ring(0, _, [First|_]) ->
  io:format("Launching ring transmission\n"),
  send_message(First, message);
setup_ring(1, M, [Next|_] = L) ->
  TailId = get_tail_process_name(M),
  Pid = spawn(?MODULE, create_head_process, [Next, TailId]),
  register(TailId, Pid),
  setup_ring(0, M, [Pid|L]);
setup_ring(N, M, []) ->
  Pid = spawn(?MODULE, create_tail_process, [get_tail_process_name(M)]),
  setup_ring(N-1, M, [Pid]);
setup_ring(N, M, [Next|_] = L) ->
  Pid = spawn(?MODULE, create_process, [Next]),
  setup_ring(N-1, M, [Pid|L]).

get_tail_process_name(M) ->
  list_to_atom(lists:flatten(io_lib:format("tail~p", [M]))).

create_process(Next) ->
  receive
    message -> send_message(Next, message), io:format("Transmitting\n")
  end.

create_tail_process(TailId) ->
  receive
    message -> send_message(TailId, stop), io:format("Tail message\n")
  end.

create_head_process(Next, TailId) ->
  receive
    message -> send_message(Next, message), io:format("Head message\n"), create_head_process(Next, TailId);
    stop -> io:format("Full circle message\n")
  end.

send_message(To, Message) ->
  To ! Message.