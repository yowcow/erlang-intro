-module(maps_test).

-include_lib("eunit/include/eunit.hrl").

create_map_test() ->
    Map = #{"foo" => "bar", "hoge" => "fuga"},
    #{"foo" := Res1, "hoge" := Res2} = Map,
    ?assertEqual("bar", Res1),
    ?assertEqual("fuga", Res2).

add_key_to_map_test() ->
    Map = #{"foo" => "bar"},
    NewMap = Map#{"hoge" => "fuga"},
    ?assertEqual(#{"foo" => "bar", "hoge" => "fuga"}, NewMap).

update_key_in_map_test() ->
    Map = #{"foo" => "bar"},
    NewMap = Map#{"foo" := "bar2"},
    ?assertEqual(#{"foo" => "bar2"}, NewMap).
