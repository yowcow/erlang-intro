-module(recursion_test).

-include_lib("eunit/include/eunit.hrl").

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

fac_test() ->
    ?assertEqual(1, fac(0)),
    ?assertEqual(1, fac(1)),
    ?assertEqual(2, fac(2)),
    ?assertEqual(6, fac(3)),
    ?assertEqual(24, fac(4)).

len([]) -> 0;
len([_ | Rest]) -> 1 + len(Rest).

len_test() ->
    ?assertEqual(0, len([])),
    ?assertEqual(1, len([1])),
    ?assertEqual(2, len([1, 1])).

tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N - 1, N * Acc).

tail_fac_test() ->
    ?assertEqual(1, tail_fac(0)),
    ?assertEqual(1, tail_fac(1)),
    ?assertEqual(2, tail_fac(2)),
    ?assertEqual(6, tail_fac(3)),
    ?assertEqual(24, tail_fac(4)).

tail_len(L) -> tail_len(L, 0).

tail_len([], Acc) -> Acc;
tail_len([_ | Rest], Acc) -> tail_len(Rest, Acc + 1).

tail_len_test() ->
    ?assertEqual(0, tail_len([])),
    ?assertEqual(1, tail_len([1])),
    ?assertEqual(2, tail_len([1, 1])).

duplicate(0, _) -> [];
duplicate(N, T) when N > 0 ->
    [T | duplicate(N - 1, T)].

duplicate_test() ->
    ?assertEqual([], duplicate(0, hoge)),
    ?assertEqual([hoge, hoge], duplicate(2, hoge)).

tail_duplicate(N, T) -> tail_duplicate(N, T, []).

tail_duplicate(0, _, Acc) -> Acc;
tail_duplicate(N, T, Acc) when N > 0 -> tail_duplicate(N - 1, T, [T | Acc]).

tail_duplicate_test() ->
    ?assertEqual([], tail_duplicate(0, hoge)),
    ?assertEqual([hoge, hoge], tail_duplicate(2, hoge)).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

reverse_test() ->
    ?assertEqual([], reverse([])),
    ?assertEqual([3,2,1], reverse([1,2,3])).

tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([H | T], Acc) -> tail_reverse(T, [H | Acc]).

tail_reverse_test() ->
    ?assertEqual([], tail_reverse([])),
    ?assertEqual([3,2,1], tail_reverse([1,2,3])).
