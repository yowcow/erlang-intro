-module(udpbroadcast).

-export([
    send/1,
    listen/1,
    stop_listen/1,
    start_listener/0,
    stop_listener/0
]).

-define(INTERFACE, "eth0").
-define(SERVER_PORT, 2345).

send(IoList) ->
    Bin = unicode:characters_to_binary(IoList, utf8),
    case inet:ifget(?INTERFACE, [broadaddr]) of
        {ok, [{broadaddr, Ip}]} ->
            {ok, Sock} = gen_udp:open(0, [{broadcast, true}]),
            gen_udp:send(Sock, Ip, ?SERVER_PORT, Bin),
            gen_udp:close(Sock);
        _ ->
            io:format(
                "Bad interface name, or~n"
                "broadcasting not supported~n"
            )
    end.

io_format_writer(IoList) -> io:format("~ts~n", [IoList]).

start_listener() ->
    Pid = spawn_link(fun() -> listen(fun io_format_writer/1) end),
    register(udpbroadcast, Pid).

stop_listener() ->
    stop_listen(whereis(udpbroadcast)).

stop_listen(undefined) -> no_listener;
stop_listen(Pid) ->
    Pid ! {quit, self()},
    receive X -> X end.

listen(Writer) ->
    erlang:display(Writer),
    {ok, _} = gen_udp:open(?SERVER_PORT, [binary]),
    loop(Writer).

loop(Writer) ->
    receive
        {udp, _, _, _, Bin} ->
            IoList = unicode:characters_to_list(Bin),
            Writer(io_lib:format("Received a broadcast: ~ts", [IoList])),
            loop(Writer);
        {quit, From} ->
            From ! ok
    end.
