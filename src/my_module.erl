-module(my_module).

-include("config.hrl").

-export([hello/0, greet/2]).

hello() ->
    io_lib:format("Hello world ~p~n", [?MYID]).

greet(male, Name) ->
    lists:flatten(io_lib:format('Hello, Mr. ~s!', [Name]));
greet(female, Name) ->
    lists:flatten(io_lib:format("Hello, Ms. ~s!", [Name]));
greet(_, Name) ->
    lists:flatten(io_lib:format("Hello, ~s!", [Name])).
