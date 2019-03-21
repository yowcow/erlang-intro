-module(udpbroadcast_tests).

-include_lib("eunit/include/eunit.hrl").

send_test() ->
    ok = udpbroadcast:send("あいうえお").

start_stop_listener_test() ->
    true = udpbroadcast:start_listener(),
    ok = udpbroadcast:stop_listener().
