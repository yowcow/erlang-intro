-module(server2).

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
        {Name, ok, Response} -> Response
    end.

loop(Name, Module, State) ->
    receive
        {From, stop} ->
            From ! {Name, ok, stopped};
        {From, Request} ->
            try Module:handle(Request, State) of
                {Response, NewState} ->
                    From ! {Name, ok, Response},
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
        "Server ~p request ~p ~n"
        "caused exception ~p~n",
        [Name, Request, Why]
    ).
