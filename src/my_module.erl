-module(my_module).

-include("config.hrl").

-export([hello/0]).

hello() ->
    io_lib:format("Hello world ~p~n", [?MYID]).
