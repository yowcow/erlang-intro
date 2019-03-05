-module(kvs_tests).

-include_lib("eunit/include/eunit.hrl").

start_stop_test_() ->
    Start = kvs:start(),
    Stop = kvs:stop(),
    [
        ?_assertEqual(true, Start),
        ?_assertEqual(bye, Stop)
    ].

store_lookup_test_() ->
    [
        ?_assertEqual(true, kvs:start()),
        ?_assertEqual(ok, kvs:store(hoge, 123)),
        ?_assertEqual(ok, kvs:store(fuga, 234)),
        ?_assertEqual(ok, kvs:store(hoge, 345)),
        ?_assertEqual({ok, 345}, kvs:lookup(hoge)),
        ?_assertEqual({ok, 234}, kvs:lookup(fuga)),
        ?_assertEqual(undefined, kvs:lookup(foo)),
        ?_assertEqual(bye, kvs:stop())
    ].
