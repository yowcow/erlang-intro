-module(hhfuns_test).

-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual(3, hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0)).

increment_test() ->
    ?assertEqual([], hhfuns:increment([])),
    ?assertEqual([2,3,4], hhfuns:increment([1,2,3])).

decrement_test() ->
    ?assertEqual([], hhfuns:decrement([])),
    ?assertEqual([0,1,2], hhfuns:decrement([1,2,3])).

incr_test() ->
    ?assertEqual([], lists:map(fun hhfuns:incr/1, [])),
    ?assertEqual([2,3,4], lists:map(fun hhfuns:incr/1, [1,2,3])).

decr_test() ->
    ?assertEqual([], lists:map(fun hhfuns:decr/1, [])),
    ?assertEqual([0,1,2], lists:map(fun hhfuns:decr/1, [1,2,3])).
