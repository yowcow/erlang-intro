-module(udpserver_tests).

-include_lib("eunit/include/eunit.hrl").

client_without_server_test() ->
    ?assertEqual(0, udpserver:client(10)).

server_start_stop_test() ->
    true = udpserver:start_server(),
    6 = udpserver:client(3),
    24 = udpserver:client(4),
    120 = udpserver:client(5),
    ok = udpserver:stop_server(),
    0 = udpserver:client(3).
