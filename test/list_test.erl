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

list_comprehension_with_tuple_test() ->
    Menu = [{steak, 5.99},
            {beer, 3.99},
            {poutine, 3.50},
            {kitten, 20.99},
            {water, 0.00}],
    Menu_over_4 = [{Item, Price} || {Item, Price} <- Menu, Price >= 4.0],
    Menu_over_4_with_tax = [{Item, Price * 1.07} || {Item, Price} <- Menu_over_4],
    ?assertEqual(
            [{steak, 5.99}, {kitten, 20.99}],
            Menu_over_4),
    ?assertEqual(
            [{steak,6.409300000000001}, {kitten,22.4593}],
            Menu_over_4_with_tax).

lists_comprehension_test() ->
    Result = [X + Y || X <- [1,2], Y <- [2,3]],
    ?assertEqual([3,4,4,5], Result).

reverse(List) ->
    reverse(List, []).

reverse([Head | Rest], Reversed) ->
    reverse(Rest, [Head | Reversed]);

reverse([], Reversed) ->
    Reversed.

reverse_test() ->
    Result = reverse([1,2,3,4]),
    ?assertEqual([4,3,2,1], Result).

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

list_max([Head | Rest], Current_max) ->
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
