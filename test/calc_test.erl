-module(calc_test).

-include_lib("eunit/include/eunit.hrl").

read_test_() ->
    [   ?_assertEqual(1.23, calc:read("1.23")),
        ?_assertEqual(123, calc:read("123"))].

rpn2_test_() ->
    [   ?_assertEqual(
            [3, 3],
            calc:rpn("+", [1, 2, 3])),
        ?_assertEqual(
            [1, 3],
            calc:rpn("-", [1, 2, 3])),
        ?_assertEqual(
            [2, 3],
            calc:rpn("*", [1, 2, 3])),
        ?_assertEqual(
            [2.0, 3],
            calc:rpn("/", [1, 2, 3])),
        ?_assertEqual(
            [2.0, 3],
            calc:rpn("^", [1, 2, 3])),
        ?_assertEqual(
            [0.6931471805599453],
            calc:rpn("ln", [2])),
        ?_assertEqual(
            [0.3010299956639812],
            calc:rpn("log10", [2])),
        ?_assertEqual(
            [1,2,3],
            calc:rpn("1", [2,3]))].

rpn_test_() ->
    [   ?_assertEqual(
            5,
            calc:rpn("2 3 +")),
        ?_assertEqual(
            87,
            calc:rpn("90 3 -")),
        ?_assertEqual(
            -4,
            calc:rpn("10 4 3 + 2 * -")),
        ?_assertEqual(
            -2.0,
            calc:rpn("10 4 3 + 2 * - 2 /")),
        ?_assert(
            true = try
                calc:rpn("90 34 12 33 55 66 + * - +")
            catch
                error:{badmatch, [_|_]} -> true
            end),
        ?_assertEqual(
            4037,
            calc:rpn("90 34 12 33 55 66 + * - + -")),
        ?_assertEqual(
            8.0,
            calc:rpn("2 3 ^")),
        ?_assertEqual(
            math:log(2.7),
            calc:rpn("2.7 ln")),
        ?_assertEqual(
            math:log10(2.7),
            calc:rpn("2.7 log10")),
        ?_assertEqual(
            50,
            calc:rpn("10 10 10 10 10 sum")),
        ?_assertEqual(
            10.0,
            calc:rpn("10 10 10 20 sum 5 /")),
        ?_assertEqual(
            1000.0,
            calc:rpn("10 10 20 0.5 prod"))].
