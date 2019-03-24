-module(event_handler).

-export([
    make/1,
    done/1,
    add_handler/2,
    event/2
]).

make(Name) ->
    register(Name, spawn(fun() -> handler(fun no_op/1) end)).

add_handler(Name, Fun) -> Name ! {add, Fun}.

event(Name, X) -> Name ! {event, X}.

done(Name) ->
    Name ! {done, self()},
    receive X -> X end.

handler(Fun) ->
    receive
        {done, From} -> From ! done;
        {add, Fun1} -> handler(Fun1);
        {event, X} ->
            (catch Fun(X)),
            handler(Fun)
    end.

no_op(_) -> void.
