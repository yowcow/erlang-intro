-module(timer_tests).

-include_lib("eunit/include/eunit.hrl").

sleeper(Pid, N) ->
    timer:sleep(N),
    Pid ! done.

sleep_test_() ->
    Cases = [
        {"sleep is shorter than timeout", 50, 100, received},
        {"sleep is longer than timeout", 100, 10, timeout}
    ],
    F = fun({Name, Sleep, Timeout, Expected}) ->
        Self = self(),
        _ = spawn_link(fun() -> sleeper(Self, Sleep) end),
        Result = receive
            done -> received
            after Timeout -> timeout
        end,
        case Result of
            timeout -> receive X -> X end;
            _       -> nothing_to_do
        end,
        {Name, ?_assertEqual(Expected, Result)}
    end,
    lists:map(F, Cases).

sleep_manager(Parent, Interval, Count, true) ->
    receive
        quit -> Parent ! Count;
        _    -> sleep_manager(Parent, Interval, Count, false)
        after 1000 -> exit(sleep_manager_timeout)
    end;
sleep_manager(Parent, Interval, Count, false) ->
    timer:sleep(Interval),
    {message_queue_len, Length} = process_info(self(), message_queue_len),
    sleep_manager(Parent, Interval, Count + 1, Length > 0).


stop(Pid) ->
    Pid ! quit,
    receive X -> X end.

sleep_or_die_test_() ->
    Parent = self(),
    Pid = spawn_link(fun() -> sleep_manager(Parent, 10, 0, false) end),
    timer:sleep(100),
    N = stop(Pid),
    [
        ?_assert(N >= 1),
        ?_assert(N =< 10)
    ].
