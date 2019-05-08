-module(binary_test).

-include_lib("eunit/include/eunit.hrl").

binary_conversion_test_() ->
    Red = 2,
    Green = 61,
    Blue = 20,
    Mem = <<Red:5, Green:6, Blue:5>>,
    <<R1:5, G1:6, B1:5>> = Mem,
    [   ?_assertEqual(2, R1),
        ?_assertEqual(61, G1),
        ?_assertEqual(20, B1)].

chunks(Bin) ->
    chunks(Bin, []).

chunks(<<H:8/binary, T/binary>>, Acc) ->
    chunks(T, [H | Acc]);
chunks(H, Acc) ->
    [H | Acc].

chunks_test() ->
    {ok, Bin} = file:read_file("data/text.txt"),
    Chunks = chunks(Bin),
    Expected = [
        <<10>>,
        <<132,161,227,129,132,227,128,130>>,
        <<175,227,129,190,227,129,160,231>>,
        <<229,144,141,229,137,141,227,129>>,
        <<129,130,227,130,139,227,128,130>>,
        <<175,231,140,171,227,129,167,227>>,
        <<229,144,190,232,188,169,227,129>>
    ],
    ?assertEqual(Expected, Chunks).

combine(Chunks) ->
    combine(Chunks, <<>>).

combine([], Acc) -> Acc;
combine([Chunk | T], Acc) ->
    combine(T, <<Chunk/binary, Acc/binary>>).

combine_test() ->
    Chunks = [
        <<10>>,
        <<132,161,227,129,132,227,128,130>>,
        <<175,227,129,190,227,129,160,231>>,
        <<229,144,141,229,137,141,227,129>>,
        <<129,130,227,130,139,227,128,130>>,
        <<175,231,140,171,227,129,167,227>>,
        <<229,144,190,232,188,169,227,129>>
    ],
    Bin = combine(Chunks),
    Expected = <<"吾輩は猫である。名前はまだ無い。\n"/utf8>>,
    ?assertEqual(Expected, Bin).
