-module(area_server).
-export([start/0, area/2, loop/0]).

start() -> spawn(area_server, loop, []).

area(Pid, Request) ->
  rpc(Pid, Request).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

loop() ->
  receive
    {From, {rectangle, W, H}} ->
      From ! {self(), W * H},
      loop();
    {From, {circle, R}} ->
      From ! {self(), 3.14 * R * R},
      loop();
    {From, Other} ->
      From ! {self(), {error, Other}},
      loop()
  end.