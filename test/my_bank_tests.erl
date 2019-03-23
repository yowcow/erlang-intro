-module(my_bank_tests).

-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, _} = my_bank:start(),
    stopped = my_bank:stop().

transaction_test_() ->
    Cases = [
        {
            "start bank",
            fun() ->
                {ok, _} = my_bank:start()
            end
        },
        {
            "deposit to non-existing account",
            fun() ->
                Actual = my_bank:deposit("joe", 100),
                ?assertEqual(not_a_customer, Actual)
            end
        },
        {
            "create joe's account",
            fun() ->
                 Actual = my_bank:new_account("joe"),
                ?assertEqual({welcome, "joe"}, Actual)
            end
        },
        {
            "create mike's account",
            fun() ->
                 Actual = my_bank:new_account("mike"),
                ?assertEqual({welcome, "mike"}, Actual)
            end
        },
        {
            "create joe's dupe account",
            fun() ->
                Actual = my_bank:new_account("joe"),
                ?assertEqual({"joe", you_already_are_a_customer}, Actual)
            end
        },
        {
            "create mike's dupe account",
            fun() ->
                Actual = my_bank:new_account("mike"),
                ?assertEqual({"mike", you_already_are_a_customer}, Actual)
            end
        },
        {
            "deposit negative amount",
            fun() ->
                Actual = my_bank:deposit("joe", -100),
                ?assertEqual(you_can_only_add_more_than_0, Actual)
            end
        },
        {
            "deposit valid amount",
            fun() ->
                Actual = my_bank:deposit("joe", 90),
                ?assertEqual({thanks, "joe", your_balance_is, 90}, Actual)
            end
        },
        {
            "deposit valid amount",
            fun() ->
                Actual = my_bank:deposit("joe", 10),
                ?assertEqual({thanks, "joe", your_balance_is, 100}, Actual)
            end
        },
        {
            "withdraw negative amount",
            fun() ->
                Actual = my_bank:withdraw("joe", -100),
                ?assertEqual(you_can_only_remove_more_than_0, Actual)
            end
        },
        {
            "withdraw amount more than balance",
            fun() ->
                Actual = my_bank:withdraw("joe", 101),
                ?assertEqual({sorry, "joe", you_only_have, 100, in_the_bank}, Actual)
            end
        },
        {
            "stop bank",
            fun() ->
                stopped = my_bank:stop()
            end
        }
    ],
    F = fun({Name, Func}) ->
        {Name, Func}
    end,
    lists:map(F, Cases).
