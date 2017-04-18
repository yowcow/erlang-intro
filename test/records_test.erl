-module(records_test).

-include_lib("eunit/include/eunit.hrl").

-include("config.hrl").

person_test() ->
    P = ?PERSON,
    ?assertEqual({person, 12345, f, 99, 0}, P),
    ?assertEqual(99, P#person.age),
    ?assertEqual(f, P#person.gender),
    ?assertEqual(12345, P#person.myid),
    ?assertEqual(0, P#person.zip).
