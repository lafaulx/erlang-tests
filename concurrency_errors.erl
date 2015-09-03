-module(concurrency_errors).
-export([my_spawn_monitor/3,
  my_spawn_on_exit/3,
  my_spawn_second_kill/3,
  dummy_endless/0,
  spawn_registered_process/0,
  spawn_registered_process_monitor/0,
  dummy_registered/0,
  spawn_worker_monitor/1,
  spawn_worker_monitor2/1]).

my_spawn_monitor(Mod, Func, Args) ->
  Ts1 = os:timestamp(),
  {Pid, Ref} = spawn_monitor(Mod, Func, Args),
  receive
    {'DOWN', Ref, process, Pid, Reason} ->
      io:format("Process exit: ~p, ~p.~n", [Reason, timer:now_diff(os:timestamp(), Ts1)])
  end.

my_spawn_on_exit(Mod, Func, Args) ->
  Ts1 = os:timestamp(),
  Pid = spawn(Mod, Func, Args),
  on_exit(Pid, fun(Reason) ->
    io:format("Process exit: ~p, ~p.~n", [Reason, timer:now_diff(os:timestamp(), Ts1)])
  end).

my_spawn_second_kill(Mod, Func, Args) ->
  {Pid, _} = spawn_monitor(Mod, Func, Args),
  receive
    after 1000 ->
      exit(Pid, kill)
  end.

spawn_registered_process() ->
  Pid = spawn(?MODULE, dummy_registered, []),
  register(dummy_reg_fn, Pid).

spawn_registered_process_monitor() ->
  spawn(fun() ->
    Pid = whereis(dummy_reg_fn),
    Ref = monitor(process, Pid),
    receive
      {'DOWN', Ref, process, Pid, Reason} ->
        io:format("Caught kill: ~p, restarting~n", [Reason]),
        spawn_registered_process()
    end
  end).

spawn_worker_monitor(Fs) ->
  spawn(fun() -> create_worker_monitor(create_workers_map(Fs, maps:new())) end).

spawn_worker_monitor2(Fs) ->
  spawn(fun() -> create_worker_monitor2(create_workers_map(Fs, maps:new())) end).

create_worker_monitor(M) ->
  io:format("Monitoring processes: ~p~n", [M]),
  receive
    {'DOWN', Ref, process, Pid, Reason} ->
      case Reason of
        normal ->
          io:format("Normal process exit: skipping~n"),
          create_worker_monitor(maps:remove({Pid, Ref}, M));
        _ ->
          io:format("Process down due an error. Respawning worker: ~p~n", [{Pid, Ref}]),
          create_worker_monitor(respawn_worker_process({Pid, Ref}, M))
      end
  end.

create_worker_monitor2(M) ->
  io:format("Monitoring processes: ~p~n", [M]),
  receive
    {'DOWN', _Ref, process, _Pid, Reason} ->
      case Reason of
        normal ->
          io:format("Normal process exit: skipping~n"),
          create_worker_monitor2(M);
        _ ->
          io:format("Process down due an error. Respawning workers.~n"),
          demonitor_and_kill_workers(maps:keys(M)),
          create_worker_monitor2(create_workers_map(maps:values(M), maps:new()))
      end
  end.

create_workers_map([], M) ->
  M;
create_workers_map([H|T], M) ->
  create_workers_map(T, maps:put(spawn_monitor(H), H, M)).

respawn_worker_process(K, M) ->
  F = maps:get(K, M),
  M1 = maps:remove(K, M),
  maps:put(spawn_monitor(F), F, M1).

demonitor_and_kill_workers([]) ->
  void;
demonitor_and_kill_workers([{Pid, Ref}|L]) ->
  demonitor(Ref),
  exit(Pid, kill),
  demonitor_and_kill_workers(L).

on_exit(Pid, Fun) ->
  spawn(fun() ->
    Ref = monitor(process, Pid),
    receive
      {'DOWN', Ref, process, Pid, Why} -> Fun(Why)
    end
  end).

dummy_endless() ->
  receive
    {'EXIT', _From, Reason} -> io:format("Got exit: ~p~n", [Reason]);
    _ -> dummy_endless()
  end.

dummy_registered() ->
  receive
    after 5000 ->
      io:format("I'm still alive~n"),
      dummy_registered()
  end.