-module(exceptions_test).

-include_lib("eunit/include/eunit.hrl").

-export([hoge/1]).

throws_test() ->
    ok = exceptions:throws(fun () -> hoge end),
    {throw, caught, hogefuga} = exceptions:throws(fun () -> throw(hogefuga) end).

errors_test() ->
    ok = exceptions:errors(fun () -> hoge end),
    {error, caught, hogefuga} = exceptions:errors(fun () -> erlang:error(hogefuga) end).

exits_test() ->
    ok = exceptions:exits(fun () -> hoge end),
    {exit, caught, hogefuga} = exceptions:exits(fun () -> exit(hogefuga) end).

black_night_test_() ->
    [?_assertEqual("None shall pass.",     exceptions:black_knight(fun exceptions:talk/0)),
     ?_assertEqual("It is but a scratch.", exceptions:black_knight(fun () -> exceptions:sword(1) end)),
     ?_assertEqual("I've had worse.",      exceptions:black_knight(fun () -> exceptions:sword(2) end)),
     ?_assertEqual("Come on you pansy!",   exceptions:black_knight(fun () -> exceptions:sword(3) end)),
     ?_assertEqual("Just a flesh wound.",  exceptions:black_knight(fun () -> exceptions:sword(4) end)),
     ?_assertEqual("Just a flesh wound.",  exceptions:black_knight(fun () -> exceptions:sword(5) end))].

catcher_test_() ->
    [?_assertEqual(1.0,     exceptions:catcher(2, 2)),
     ?_assertEqual("uh oh", exceptions:catcher(2, 0))].

hoge({Foo}) -> Foo.

catch_test() ->
    case catch hoge([hoge]) of
        {'EXIT', _} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end,
    case catch hoge({hoge}) of
        {'EXIT', _} ->
            ?assert(false);
        Result ->
            ?assertEqual(hoge, Result)
    end.
