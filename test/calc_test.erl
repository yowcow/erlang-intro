-module(calc_test).

-include_lib("eunit/include/eunit.hrl").

read_test() ->
    ?assertEqual(1.23, calc:read("1.23")),
    ?assertEqual(123, calc:read("123")).

rpn2_test() ->
    ?assertEqual(
        [3, 3],
        calc:rpn("+", [1, 2, 3])),
    ?assertEqual(
        [1, 3],
        calc:rpn("-", [1, 2, 3])),
    ?assertEqual(
        [2, 3],
        calc:rpn("*", [1, 2, 3])),
    ?assertEqual(
        [2.0, 3],
        calc:rpn("/", [1, 2, 3])),
    ?assertEqual(
        [2.0, 3],
        calc:rpn("^", [1, 2, 3])),
    ?assertEqual(
        [0.6931471805599453],
        calc:rpn("ln", [2])),
    ?assertEqual(
        [0.3010299956639812],
        calc:rpn("log10", [2])),
    ?assertEqual(
        [1,2,3],
        calc:rpn("1", [2,3])).

rpn_test() ->
    ?assertEqual(
        5,
        calc:rpn("2 3 +")),
    ?assertEqual(
        87,
        calc:rpn("90 3 -")),
    ?assertEqual(
        -4,
        calc:rpn("10 4 3 + 2 * -")),
    ?assertEqual(
        -2.0,
        calc:rpn("10 4 3 + 2 * - 2 /")),
    ?assert(
        true = try
            calc:rpn("90 34 12 33 55 66 + * - +")
        catch
            error:{badmatch, [_|_]} -> true
        end),
    ?assertEqual(
        4037,
        calc:rpn("90 34 12 33 55 66 + * - + -")),
    ?assertEqual(
        8.0,
        calc:rpn("2 3 ^")),
    ?assertEqual(
        math:log(2.7),
        calc:rpn("2.7 ln")),
    ?assertEqual(
        math:log10(2.7),
        calc:rpn("2.7 log10")),
    ?assertEqual(
        50,
        calc:rpn("10 10 10 10 10 sum")),
    ?assertEqual(
        10.0,
        calc:rpn("10 10 10 20 sum 5 /")),
    ?assertEqual(
        1000.0,
        calc:rpn("10 10 20 0.5 prod")).
