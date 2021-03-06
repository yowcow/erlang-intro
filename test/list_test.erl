-module(list_test).

-include_lib("eunit/include/eunit.hrl").

combine_test() ->
    List = [1,2,3] ++ [4,5],
    ?assertEqual([1,2,3,4,5], List).

subtract_test() ->
    List = [1,2,3] -- [2,3],
    ?assertEqual([1], List).

td_test() ->
    First = hd([1,2,3,4]),
    ?assertEqual(1, First).

tl_test() ->
    Tail = tl([1,2,3,4]),
    ?assertEqual([2,3,4], Tail).

head_and_rest_test() ->
    [Head | Rest] = [1,2,3,4],
    ?assertEqual(1, Head),
    ?assertEqual([2,3,4], Rest).

append_list_to_list_test() ->
    Rest = [2,3,4],
    List = [1 | Rest],
    ?assertEqual([1,2,3,4], List).

list_comprehension_test() ->
    List = [1,2,3,4],
    Result = [2 * N || N <- List],
    ?assertEqual([2,4,6,8], Result).

list_comprehension_with_constraint_test() ->
    List = [1,2,3,4,5,6,7,8,9,10],
    Result = [X || X <- List, X rem 2 =:= 0],
    ?assertEqual([2,4,6,8,10], Result).

list_comprehension_with_tuple_test_() ->
    Menu = [{steak, 5.99},
            {beer, 3.99},
            {poutine, 3.50},
            {kitten, 20.99},
            {water, 0.00}],
    Menu_over_4 = [{Item, Price} || {Item, Price} <- Menu, Price >= 4.0],
    Menu_over_4_with_tax = [{Item, Price * 1.07} || {Item, Price} <- Menu_over_4],
    [   ?_assertEqual(
                [{steak, 5.99}, {kitten, 20.99}],
                Menu_over_4),
        ?_assertEqual(
                [{steak,6.409300000000001}, {kitten,22.4593}],
                Menu_over_4_with_tax)].

lists_comprehension_test() ->
    Result = [X + Y || X <- [1,2], Y <- [2,3]],
    ?assertEqual([3,4,4,5], Result).

%% odds_and_evens/1, odds_end_evens/3
odds_and_evens(L) ->
    odds_and_evens(L, [], []).

odds_and_evens([], Odds, Evens) ->
    {lists:reverse(Odds), lists:reverse(Evens)};
odds_and_evens([H | T], Odds, Evens) ->
    case H rem 2 of
        1 -> odds_and_evens(T, [H | Odds], Evens);
        0 -> odds_and_evens(T, Odds, [H | Evens])
    end.

odds_and_evens_test() ->
    ?assertEqual(
        {[1,3,5,7], [2,4,6,8]},
        odds_and_evens(lists:seq(1, 8))).

%% reverse/1, reverse/2
reverse(List) ->
    reverse(List, []).

reverse([Head | Rest], Reversed) ->
    reverse(Rest, [Head | Reversed]);

reverse([], Reversed) ->
    Reversed.

reverse_test() ->
    Result = reverse([1,2,3,4]),
    ?assertEqual([4,3,2,1], Result).

%% list_max/1, list_max/2
list_max([Head | Rest]) ->
    list_max(Rest, Head).

%list_max([Head | Rest], Current_max) ->
%    if
%        Head > Current_max ->
%            list_max(Rest, Head);
%        true ->
%            list_max(Rest, Current_max)
%    end;

list_max([Head | Rest], Current_max) when Head > Current_max ->
    list_max(Rest, Head);

list_max([_ | Rest], Current_max) ->
    list_max(Rest, Current_max);

list_max([], Current_max) ->
    Current_max.

list_max_test() ->
    Result = list_max([1,2,3,4,3,2,1]),
    ?assertEqual(4, Result).

list_map_test() ->
    F = fun(X) ->
        2 * X
    end,
    List = [1,2,3,4],
    Result = lists:map(F, List),
    ?assertEqual([2,4,6,8], Result).

discount_by_50_pct({price, X}) ->
    {price, X / 2}.

list_map_function_test() ->
    Price_list = [{price, 10}, {price, 20}, {price, 30}],
    Result = lists:map(fun discount_by_50_pct/1, Price_list),
    ?assertEqual([{price, 5.0}, {price, 10.0}, {price, 15.0}], Result).

list_seq_test() ->
    Result = lists:seq(1, 4),
    ?assertEqual([1,2,3,4], Result).

list_flatten_test() ->
    Result = lists:flatten([[1,2], [[3]], [[[4]]]]),
    ?assertEqual([1,2,3,4], Result).

%% sum/1, sum/2
sum(L) -> sum(L, 0).

sum([], Sum) -> Sum;
sum([H | T], Sum) -> sum(T, Sum + H).

sum_test() ->
    ?assertEqual(6, sum([1,2,3])).

%% seq/2, seq/3
seq(From, To) -> seq(From, To, []).

seq(From, From, List) -> [From | List];
seq(From, To, List) when From < To -> seq(From, To - 1, [To | List]);
seq(_, _, _) -> throw(infinite_seq).

seq_test_() ->
    [   ?_assertEqual([1], seq(1, 1)),
        ?_assertEqual([1,2], seq(1, 2)),
        ?_assertEqual([1,2,3], seq(1, 3))].

seq_throw_test() ->
    try seq(2, 1) of
        _ -> ?assert(false)
    catch
        Thrown -> ?assertEqual(infinite_seq, Thrown)
    end.

list_foldl_test() ->
    Sum = lists:foldl(fun (X, Sum) -> X + Sum end, 0, [1,2,3,4]),
    Prd = lists:foldl(fun (X, Prd) -> X * Prd end, 1, [1,2,3,4]),
    [   ?_assertEqual(10, Sum),
        ?_assertEqual(25, Prd)].

%% for/3, for/4
for(Min, Max, F) ->
    for(Min, Max, F, []).

for(Min, Min, F, Acc) ->
    [F(Min) | Acc];
for(Min, Max, F, Acc) ->
    for(Min, Max - 1, F, [F(Max) | Acc]).

for_test_() ->
    [   ?_assertEqual([1,2,3,4,5], for(1, 5, fun(X) -> X end)),
        ?_assertEqual([2], for(1, 1, fun (X) -> X * 2 end))].
