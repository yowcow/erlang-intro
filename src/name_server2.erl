-module(name_server2).

-export([
    init/0,
    handle/2,
    add/2,
    whereis/1,
    fail/0
]).

-import(server2, [rpc/2]).

init() -> dict:new().

handle({add, Name, Place}, Dict) ->
    {ok, dict:store(Name, Place, Dict)};
handle({whereis, Name}, Dict) ->
    {dict:find(Name, Dict), Dict};
handle({fail}, _) ->
    exit(something_has_gone_wrong).

add(Name, Place) ->
    rpc(name_server, {add, Name, Place}).

whereis(Name) ->
    rpc(name_server, {whereis, Name}).

fail() ->
    rpc(name_server, {fail}).
