-module(tuple_test).

-include_lib("eunit/include/eunit.hrl").

tuple_test_() ->
    Point = {4, 5},
    {X, Y} = Point,
    [   ?_assertEqual(X, 4),
        ?_assertEqual(Y, 5)].

tuple_in_tuple_test() ->
    X = 4,
    Y = 5,
    Point = {X, Y},
    Item = {point, Point},
    ?assertEqual({point, {4, 5}}, Item).
