-module(exceptions_test).

-include_lib("eunit/include/eunit.hrl").

throws_test() ->
    ok = exceptions:throws(fun () -> hoge end),
    {throw, caught, hogefuga} = exceptions:throws(fun () -> throw(hogefuga) end).

errors_test() ->
    ok = exceptions:errors(fun () -> hoge end),
    {error, caught, hogefuga} = exceptions:errors(fun () -> erlang:error(hogefuga) end).

exits_test() ->
    ok = exceptions:exits(fun () -> hoge end),
    {exit, caught, hogefuga} = exceptions:exits(fun () -> exit(hogefuga) end).

black_night_test() ->
    ?assertEqual("None shall pass.",     exceptions:black_knight(fun exceptions:talk/0)),
    ?assertEqual("It is but a scratch.", exceptions:black_knight(fun () -> exceptions:sword(1) end)),
    ?assertEqual("I've had worse.",      exceptions:black_knight(fun () -> exceptions:sword(2) end)),
    ?assertEqual("Come on you pansy!",   exceptions:black_knight(fun () -> exceptions:sword(3) end)),
    ?assertEqual("Just a flesh wound.",  exceptions:black_knight(fun () -> exceptions:sword(4) end)),
    ?assertEqual("Just a flesh wound.",  exceptions:black_knight(fun () -> exceptions:sword(5) end)).

catcher_test() ->
    ?assertEqual(1.0,     exceptions:catcher(2, 2)),
    ?assertEqual("uh oh", exceptions:catcher(2, 0)).
