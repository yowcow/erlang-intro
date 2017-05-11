-module(file_io_test).

-include_lib("eunit/include/eunit.hrl").

read_test_() ->
    Modes = [read, binary],
    {ok, IoDevice} = file:open("data/hoge.txt", Modes),
    [?_assertEqual({ok, <<"aaa\n">>}, file:read_line(IoDevice)),
     ?_assertEqual({ok, unicode:characters_to_binary("bbb\n")}, file:read_line(IoDevice)),
     ?_assertEqual({ok, unicode:characters_to_binary("あああ\n")}, file:read_line(IoDevice)),
     ?_assertEqual(eof, file:read_line(IoDevice))].

write_test_() ->
    Modes = [write, binary, sync],
    {ok, IoDevice} = file:open("data/hoge.txt.tmp", Modes),
    [?_assertEqual(ok, file:write(IoDevice, <<"aaa\n">>)),
     ?_assertEqual(ok, file:write(IoDevice, unicode:characters_to_binary("bbb\n"))),
     ?_assertEqual(ok, file:write(IoDevice, unicode:characters_to_binary("あああ\n"))),
     ?_assertEqual(ok, file:sync(IoDevice)),
     fun() ->
        HogeContent = file:read_file("data/hoge.txt"),
        TempContent = file:read_file("data/hoge.txt.tmp"),
        ?assertEqual(HogeContent, TempContent)
     end].
