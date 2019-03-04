-module(parallel_tests).

-include_lib("eunit/include/eunit.hrl").

echo() ->
    receive
        {From, Message} ->
            From ! {self(), Message}
    end.

receive_without_timeout() ->
    receive
        Message -> Message
    end.

spawn_receive_test() ->
    Pid = spawn(fun echo/0),
    Pid ! {self(), hello_world},
    {From, Message} = receive_without_timeout(),
    ?assertEqual(Pid, From),
    ?assertEqual(hello_world, Message).

dead_echo() ->
    receive
        _ -> i_am_dead
    end.

receive_with_timeout() ->
    receive
        Message -> Message
    after 50 -> timed_out
    end.

spawn_receive_timeout_test() ->
    Pid = spawn(fun dead_echo/0),
    Pid ! {self(), hello_world},
    Ret = receive_with_timeout(),
    ?assertEqual(timed_out, Ret).

start_echo() ->
    Name = myechopid,
    Pid = spawn(fun echo/0),
    register(Name, Pid),
    Name.

register_test_() ->
    Name = start_echo(),
    Pid0 = whereis(Name),
    Pid0 ! {self(), hello_world},
    {_, Actual} = receive_without_timeout(),
    Pid1 = whereis(Name),
    [
        ?_assertEqual(hello_world, Actual),
        ?_assertEqual(undefined, Pid1)
    ].

on_exit(Caller, Pid, Fun) ->
    spawn(fun() ->
        process_flag(trap_exit, true),
        link(Pid),
        Caller ! ok, %% notify caller now to make sure on_exit handler is ready and linked
        receive
            {'EXIT', Pid, Why} ->
                Fun(Pid, Why)
        end
    end).

on_exit_test_() ->
    Pid = spawn(fun() ->
        receive
            X -> X + 1 %% this should fail
        end
    end),
    Self = self(),
    on_exit(Self, Pid, fun(P, Why) ->
        Self ! {P, Why}
    end),
    Ready = receive %% wait for on_exit to get ready
        Ok -> Ok
    end,
    Pid ! hello,
    {FromPid, {badarith, _}} = receive
        Why -> Why
    end,
    [
        ?_assertEqual(ok, Ready),
        ?_assertEqual(Pid, FromPid)
    ].
