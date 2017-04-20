-module(proplist_test).

-include_lib("eunit/include/eunit.hrl").

get_value_from_empty_list_test() ->
    ?assertEqual(
        undefined,
        proplists:get_value(hoge, [])).

get_value_from_proplist_test() ->
    ?assertEqual(
        fuga1,
        proplists:get_value(fuga, [{hoge, hoge1}, {fuga, fuga1}])).
