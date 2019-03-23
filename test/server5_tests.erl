-module(server5_tests).

-include_lib("eunit/include/eunit.hrl").

start_wait_stop_test() ->
    true = server5:start(),
    waiting = server5:rpc(hoge),
    waiting = server5:rpc(fuga),
    stopped = server5:stop().

fac_loop() ->
    receive
        {From, {become, F}} ->
            From ! loaded,
            F();
        {From, stop} ->
            From ! stopped;
        {From, X} ->
            From ! fac(X),
            fac_loop()
    end.

fac(N) -> fac(N, 1).

fac(0, F) -> F;
fac(N, F) -> fac(N - 1, F * N).

sum_loop() ->
    receive
        {From, {become, F}} ->
            From ! loaded,
            F();
        {From, stop} ->
            From ! stopped;
        {From, X} ->
            From ! sum(X),
            sum_loop()
    end.

sum(N) -> sum(N, 0).

sum(0, S) -> S;
sum(N, S) -> sum(N - 1, S + N).

start_wait_become_stop_test_() ->
    true = server5:start(),
    waiting = server5:rpc(2),
    loaded = server5:rpc({become, fun fac_loop/0}),
    Fac = [
        server5:rpc(2),
        server5:rpc(3),
        server5:rpc(4),
        server5:rpc(5)
    ],
    loaded = server5:rpc({become, fun sum_loop/0}),
    Sum = [
        server5:rpc(2),
        server5:rpc(3),
        server5:rpc(4),
        server5:rpc(5)
    ],
    stopped = server5:stop(),
    [
        ?_assertEqual([2, 6, 24, 120], Fac),
        ?_assertEqual([3, 6, 10, 15], Sum)
    ].
