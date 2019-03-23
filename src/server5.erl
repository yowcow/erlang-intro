-module(server5).

-export([
    start/0,
    stop/0,
    rpc/1
]).

start() ->
    register(server5, spawn(fun() -> wait() end)).

stop() ->
    rpc(stop).

wait() ->
    receive
        {From, {become, F}} ->
            From ! loaded,
            F();
        {From, stop} ->
            From ! stopped;
        {From, _} ->
            From ! waiting,
            wait()
    end.

rpc(Request) ->
    server5 ! {self(), Request},
    receive X -> X end.
