-module(server3).

-export([
    start/2,
    stop/1,
    rpc/2
]).

start(Name, Module) ->
    register(Name, spawn(fun() -> loop(Name, Module, Module:init()) end)).

stop(Name) ->
    rpc(Name, stop).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {Name, crash} -> crashed;
        {Name, Response} -> Response
    end.

loop(Name, Module, State) ->
    receive
        {From, stop} ->
            From ! {Name, stopped};
        {From, {swap_code, NewModule}} ->
            From ! {Name, ack},
            loop(Name, NewModule, State);
        {From, Request} ->
            try Module:handle(Request, State) of
                {Response, NewState} ->
                    From ! {Name, Response},
                    loop(Name, Module, NewState)
            catch
                _:Why ->
                    log_the_error(Name, Request, Why),
                    From ! {Name, crash},
                    loop(Name, Module, State)
            end
    end.

log_the_error(Name, Request, Why) ->
    io:format(
        "Server ~p request ~p caused exception ~p~n",
        [Name, Request, Why]
    ).
