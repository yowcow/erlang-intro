-module(records_test).

-include_lib("eunit/include/eunit.hrl").

-include("config.hrl").

record_fields_test() ->
    ?assertEqual([myid, gender, age, zip], record_info(fields, person)).

record_size_test() ->
    ?assertEqual(5, record_info(size, person)).

record_index_test_() ->
    [   ?_assertEqual(#person.myid,   2),
        ?_assertEqual(#person.gender, 3),
        ?_assertEqual(#person.age,    4),
        ?_assertEqual(#person.zip,    5)].

person_test_() ->
    P = ?PERSON,
    [   ?_assertEqual({person, 12345, f, 99, 0}, P),
        ?_assertEqual(99,    P#person.age),
        ?_assertEqual(f,     P#person.gender),
        ?_assertEqual(12345, P#person.myid),
        ?_assertEqual(0,     P#person.zip)].

-record(nrec0, {name = "nested0"}).
-record(nrec1, {name = "nested1", nrec0 = #nrec0{}}).
-record(nrec2, {name = "nested2", nrec1 = #nrec1{}}).

nrec0_test() ->
    N0 = #nrec0{},
    ?assertEqual("nested0", N0#nrec0.name).

nrec1_test_() ->
    N1 = #nrec1{},
    [   ?_assertEqual("nested1", N1#nrec1.name),
        ?_assertEqual("nested0", (N1#nrec1.nrec0)#nrec0.name)].

nrec2_test_() ->
    N2 = #nrec2{},
    [   ?_assertEqual("nested1", (N2#nrec2.nrec1)#nrec1.name),
        ?_assertEqual("nested0", ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0.name)].
