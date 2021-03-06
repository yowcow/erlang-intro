-module(name_server1).

-export([
    init/0,
    handle/2,
    add/2,
    whereis/1
]).

-import(server1, [rpc/2]).

init() -> dict:new().

handle({add, Name, Place}, Dict) ->
    {ok, dict:store(Name, Place, Dict)};
handle({whereis, Name}, Dict) ->
    {dict:find(Name, Dict), Dict}.

add(Name, Place) ->
    rpc(name_server, {add, Name, Place}).

whereis(Name) ->
    rpc(name_server, {whereis, Name}).
