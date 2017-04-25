-module(recursion_test).

-include_lib("eunit/include/eunit.hrl").

%% fac()
fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

fac_test_() ->
    [   ?_assertEqual(1, fac(0)),
        ?_assertEqual(1, fac(1)),
        ?_assertEqual(2, fac(2)),
        ?_assertEqual(6, fac(3)),
        ?_assertEqual(24, fac(4))].

%% len()
len([]) -> 0;
len([_ | Rest]) -> 1 + len(Rest).

len_test_() ->
    [   ?_assertEqual(0, len([])),
        ?_assertEqual(1, len([1])),
        ?_assertEqual(2, len([1, 1]))].

%% tail_fac()
tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N - 1, N * Acc).

tail_fac_test_() ->
    [   ?_assertEqual(1, tail_fac(0)),
        ?_assertEqual(1, tail_fac(1)),
        ?_assertEqual(2, tail_fac(2)),
        ?_assertEqual(6, tail_fac(3)),
        ?_assertEqual(24, tail_fac(4))].

%% tail_len
tail_len(L) -> tail_len(L, 0).

tail_len([], Acc) -> Acc;
tail_len([_ | Rest], Acc) -> tail_len(Rest, Acc + 1).

tail_len_test_() ->
    [   ?_assertEqual(0, tail_len([])),
        ?_assertEqual(1, tail_len([1])),
        ?_assertEqual(2, tail_len([1, 1]))].

%% duplicate()
duplicate(0, _) -> [];
duplicate(N, T) when N > 0 ->
    [T | duplicate(N - 1, T)].

duplicate_test_() ->
    [   ?_assertEqual([], duplicate(0, hoge)),
        ?_assertEqual([hoge, hoge], duplicate(2, hoge))].

%% tail_duplicate()
tail_duplicate(N, T) -> tail_duplicate(N, T, []).

tail_duplicate(0, _, Acc) -> Acc;
tail_duplicate(N, T, Acc) when N > 0 -> tail_duplicate(N - 1, T, [T | Acc]).

tail_duplicate_test_() ->
    [   ?_assertEqual([], tail_duplicate(0, hoge)),
        ?_assertEqual([hoge, hoge], tail_duplicate(2, hoge))].

%% reverse()
reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

reverse_test_() ->
    [   ?_assertEqual([], reverse([])),
        ?_assertEqual([3,2,1], reverse([1,2,3]))].

%% tail_reverse
tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([H | T], Acc) -> tail_reverse(T, [H | Acc]).

tail_reverse_test_() ->
    [   ?_assertEqual([], tail_reverse([])),
        ?_assertEqual([3,2,1], tail_reverse([1,2,3]))].
