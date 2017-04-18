-module(records_test).

-include_lib("eunit/include/eunit.hrl").

-include("config.hrl").

record_fields_test() ->
    ?assertEqual([myid, gender, age, zip], record_info(fields, person)).

record_size_test() ->
    ?assertEqual(5, record_info(size, person)).

record_index_test() ->
    ?assertEqual(#person.myid,   2),
    ?assertEqual(#person.gender, 3),
    ?assertEqual(#person.age,    4),
    ?assertEqual(#person.zip,    5).

person_test() ->
    P = ?PERSON,
    ?assertEqual({person, 12345, f, 99, 0}, P),
    ?assertEqual(99,    P#person.age),
    ?assertEqual(f,     P#person.gender),
    ?assertEqual(12345, P#person.myid),
    ?assertEqual(0,     P#person.zip).

-record(nrec0, {name = "nested0"}).
-record(nrec1, {name = "nested1", nrec0 = #nrec0{}}).
-record(nrec2, {name = "nested2", nrec1 = #nrec1{}}).

nrec0_test() ->
    N0 = #nrec0{},
    ?assertEqual("nested0", N0#nrec0.name).

nrec1_test() ->
    N1 = #nrec1{},
    ?assertEqual("nested1", N1#nrec1.name),
    ?assertEqual("nested0", (N1#nrec1.nrec0)#nrec0.name).

nrec2_test() ->
    N2 = #nrec2{},
    ?assertEqual("nested1", (N2#nrec2.nrec1)#nrec1.name),
    ?assertEqual("nested0", ((N2#nrec2.nrec1)#nrec1.nrec0)#nrec0.name).
