-module(server3_tests).

-include_lib("eunit/include/eunit.hrl").

-import(server3, [rpc/2]).

start_handle_stop_test() ->
    Name = test_server,
    true = server3:start(Name, server3_1),
    ok = rpc(Name, incr),
    ok = rpc(Name, incr),
    crashed = rpc(Name, decr),
    Count = rpc(Name, get),
    stopped = server3:stop(Name),
    ?assertEqual(2, Count).

start_handle_swap_stop_test() ->
    Name = test_server,
    true = server3:start(Name, server3_1),
    ok = rpc(Name, incr),
    ok = rpc(Name, incr),
    ok = rpc(Name, incr),
    crashed = rpc(Name, decr),
    ack = rpc(Name, {swap_code, server3_2}),
    ok = rpc(Name, incr),
    ok = rpc(Name, decr),
    Count = rpc(Name, get),
    stopped = server3:stop(Name),
    ?assertEqual(3, Count).
