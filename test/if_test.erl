-module(if_test).

-include_lib("eunit/include/eunit.hrl").

simple_condition(X) ->
    if
        X =:= 1 -> true;
        true -> false
    end.

simple_condition_test() ->
    ?assertEqual(true, simple_condition(1)),
    ?assertEqual(false, simple_condition(2)).

simple_and_condition(X, Y) ->
    if
        X =:= 1 andalso Y =:= 1 -> true;
        true -> false
    end.

simple_and_condition_test() ->
    ?assertEqual(true, simple_and_condition(1, 1)),
    ?assertEqual(false, simple_and_condition(1, 2)).

simple_or_condition(X, Y) ->
    if
        X =:= 1 orelse Y =:= 1 -> true;
        true -> false
    end.

simple_or_condition_test() ->
    ?assertEqual(true, simple_or_condition(1, 1)),
    ?assertEqual(true, simple_or_condition(1, 2)),
    ?assertEqual(true, simple_or_condition(2, 1)),
    ?assertEqual(false , simple_or_condition(2, 2)).

help_me(Animal) ->
    Talk = if
        Animal == cat  -> "meow";
        Animal == beef -> "mooo";
        Animal == dog  -> "bark";
        Animal == tree -> "bark";
        true -> "hogefuga"
    end,
    {Animal, "says " ++ Talk ++ "!"}.

help_me_test() ->
    ?assertEqual({cat, "says meow!"}, help_me(cat)),
    ?assertEqual({tree, "says bark!"}, help_me(tree)),
    ?assertEqual({hoge, "says hogefuga!"}, help_me(hoge)).

beach(Temp) ->
    case Temp of
        {c, N} when N >= 20 andalso N =< 45 ->
            "favorable";
        {k, N} when N >= 293 andalso N =< 318 ->
            "scientifically favorable";
        {f, N} when N >= 68 andalso N =< 113 ->
            "favorable in the US";
        _ ->
            "avoid beach"
    end.

beach_with_c_test() ->
    ?assertEqual("avoid beach", beach({c, 19})),
    ?assertEqual("favorable", beach({c, 20})),
    ?assertEqual("favorable", beach({c, 45})),
    ?assertEqual("avoid beach", beach({c, 46})).

beach_with_k_test() ->
    ?assertEqual("avoid beach", beach({k, 292})),
    ?assertEqual("scientifically favorable", beach({k, 293})),
    ?assertEqual("scientifically favorable", beach({k, 318})),
    ?assertEqual("avoid beach", beach({k, 319})).

beach_with_f_test() ->
    ?assertEqual("avoid beach", beach({f, 67})),
    ?assertEqual("favorable in the US", beach({f, 68})),
    ?assertEqual("favorable in the US", beach({f, 113})),
    ?assertEqual("avoid beach", beach({f, 114})).
