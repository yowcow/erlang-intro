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

fold_test() ->
    [H | T] = [1,7,3,5,9,0,2,3],
    R1 = hhfuns:fold(fun (A, B) when A > B -> A; (_, B) -> B end, H, T),
    R2 = hhfuns:fold(fun (A, B) when A < B -> A; (_, B) -> B end, H, T),
    R3 = hhfuns:fold(fun (A, B) -> A + B end, 0, lists:seq(1, 6)),
    ?assertEqual(9, R1),
    ?assertEqual(0, R2),
    ?assertEqual(21, R3).
