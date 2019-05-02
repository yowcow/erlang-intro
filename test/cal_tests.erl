-module(cal_tests).

-include_lib("eunit/include/eunit.hrl").

now_to_datetime_test() ->
    Now = {1556,759771,346586}, % 2019-05-02 10:16:11 UTC+09
    ?assertEqual({{2019,5,2},{1,16,11}}, calendar:now_to_datetime(Now)).

now_to_universal_time_test() ->
    Now = {1556,759771,346586}, % 2019-05-02 10:16:11 UTC+09
    ?assertEqual({{2019,5,2},{1,16,11}}, calendar:now_to_universal_time(Now)).

rfc3339_to_system_time_test_() ->
    Cases = [
        {
            "2019-05-02T01:23:55.00Z",
            [],
            1556760235
        },
        {
            "2019-05-02T10:23:55.00+09:00",
            [],
            1556760235
        },
        {
            "2019-05-02 10:23:55.00+09:00",
            [],
            1556760235
        },
        {
            "2019-05-02T10:23:55.00+09:00",
            [{unit, nanosecond}],
            1556760235000000000
        }
    ],
    F = fun({Input, Opt, Expected}) ->
        Actual = calendar:rfc3339_to_system_time(Input, Opt),
        {Input, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

system_time_to_rfc3339_test_() ->
    Cases = [
        {
            1556760235,
            [{offset, "Z"}],
            "2019-05-02T01:23:55Z"
        },
        {
            1556760235,
            [{offset, "+09:00"}],
            "2019-05-02T10:23:55+09:00"
        },
        {
            1556760235,
            [{time_designator, $\s}, {offset, "+09:00"}],
            "2019-05-02 10:23:55+09:00"
        },
        {
            1556760235000,
            [{unit, millisecond}, {offset, "+09:00"}],
            "2019-05-02T10:23:55.000+09:00"
        }
    ],
    F = fun({Input, Opt, Expected}) ->
        Actual = calendar:system_time_to_rfc3339(Input, Opt),
        {Expected, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

last_day_of_the_month_test_() ->
    Cases = [
        {
            "non-leap year",
            {2019, 2},
            28
        },
        {
            "leap year",
            {2020, 2},
            29
        }
    ],
    F = fun({Name, {Year, Month}, Expected}) ->
        Actual = calendar:last_day_of_the_month(Year, Month),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).
