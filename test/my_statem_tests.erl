-module(my_statem_tests).

-include_lib("eunit/include/eunit.hrl").

push_test_() ->
    {setup,
     fun() ->
             my_statem:start()
     end,
     fun({ok, _}) ->
             my_statem:stop()
     end,
     fun(_) ->
             Cases = [
                     {
                      "on after 1st push",
                      on,
                      1
                     },
                     {
                      "off after 2nd push",
                      off,
                      1
                     },
                     {
                      "on after 3rd push",
                      on,
                      2
                     },
                     {
                      "off after 4th push",
                      off,
                      2
                     }
                    ],
             F = fun({Title, ExpectedState, ExpectedCount}) ->
                         ActualState = my_statem:push(),
                         ActualCount = my_statem:get_count(),
                         [
                          {Title ++ ": state", ?_assertEqual(ExpectedState, ActualState)},
                          {Title ++ ": count", ?_assertEqual(ExpectedCount, ActualCount)}
                         ]
                 end,
             lists:map(F, Cases)
     end}.
