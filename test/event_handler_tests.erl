-module(event_handler_tests).

-include_lib("eunit/include/eunit.hrl").

make_done_test() ->
    true = event_handler:make(?MODULE),
    {event, hoge} = event_handler:event(?MODULE, hoge),
    done = event_handler:done(?MODULE).

loop(Msgs) ->
    receive
        {add, Msg} ->
            loop([Msg | Msgs]);
        {quit, From} ->
            From ! lists:reverse(Msgs)
    end.

add_msg(Pid, Msg) ->
    Pid ! {add, Msg}.

quit_msg(Pid) ->
    Pid ! {quit, self()},
    receive X -> X end.

add_handler_test() ->
    Pid = spawn_link(fun() -> loop([]) end),
    true = event_handler:make(?MODULE),
    F = fun(X) -> add_msg(Pid, X) end,
    event_handler:event(?MODULE, hoge1),
    event_handler:event(?MODULE, hoge2),
    event_handler:add_handler(?MODULE, F),
    event_handler:event(?MODULE, hoge3),
    event_handler:event(?MODULE, hoge4),
    done = event_handler:done(?MODULE),
    Msgs = quit_msg(Pid),
    ?assertEqual([hoge3, hoge4], Msgs).
