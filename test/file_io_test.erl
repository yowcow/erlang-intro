-module(file_io_test).

-include_lib("eunit/include/eunit.hrl").

read_test_() ->
    Modes = [read, binary],
    {ok, IoDevice} = file:open("data/hoge.txt", Modes),
    [?_assertEqual({ok, <<"aaa\n">>}, file:read_line(IoDevice)),
     ?_assertEqual({ok, unicode:characters_to_binary("bbb\n")}, file:read_line(IoDevice)),
     ?_assertEqual({ok, unicode:characters_to_binary("あああ\n")}, file:read_line(IoDevice)),
     ?_assertEqual(eof, file:read_line(IoDevice))].

pread_test_() ->
    {ok, S} = file:open("data/hoge.txt", [read, binary, raw]),
    {ok, Word1} = file:pread(S, 0, 3),
    {ok, Word2} = file:pread(S, 4, 3),
    {ok, Word3} = file:pread(S, 8, 3),
    {ok, Word4} = file:pread(S, 11, 6),
    {ok, Last} = file:pread(S, 11, 10),
    Close = file:close(S),
    [
        ?_assertEqual(<<"aaa">>, Word1),
        ?_assertEqual(<<"bbb">>, Word2),
        ?_assertEqual(<<"あ"/utf8>>, Word3),
        ?_assertEqual(<<"ああ"/utf8>>, Word4),
        ?_assertEqual(<<"ああ\n"/utf8>>, Last),
        ?_assertEqual(ok, Close)
    ].

pread_utf8_test_() ->
    {ok, S} = file:open("data/hoge.txt", [read, utf8, raw]),
    {ok, Word3} = file:pread(S, 8, 3),
    {ok, Word4} = file:pread(S, 11, 6),
    {ok, Last} = file:pread(S, 11, 10),
    Close = file:close(S),
    [
        ?_assertEqual(binary_to_list(<<"あ"/utf8>>), Word3),
        ?_assertEqual(binary_to_list(<<"ああ"/utf8>>), Word4),
        ?_assertEqual(binary_to_list(<<"ああ\n"/utf8>>), Last),
        ?_assertEqual(ok, Close)
    ].

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

io_read_test_() ->
    {ok, S} = file:open("data/fuga.dat", read),
    {ok, Line1} = io:read(S, ''),
    {ok, Line2} = io:read(S, ''),
    Eof = io:read(S, ''),
    Close = file:close(S),
    [
        ?_assertEqual({person, "joe", "armstrong", [{occupation, programmer}, {favoriteLanguage, erlang}]}, Line1),
        ?_assertEqual({cat, {name, "zorro"}, {owner, "joe"}}, Line2),
        ?_assertEqual(eof, Eof),
        ?_assertEqual(ok, Close)
    ].

start_writer(F) ->
    register(writer, spawn_link(fun() ->
        {ok, S} = file:open(F, write),
        loop_writer(S)
    end)),
    ok.

loop_writer(S) ->
    receive
        {write, Msg} ->
            io:format(S, "~p.~n", [Msg]),
            loop_writer(S);
        {quit, From} ->
            From ! file:close(S)
    end.

write_writer(Msg) ->
    writer ! {write, Msg}.

quit_writer() ->
    writer ! {quit, self()},
    receive
        {'EXIT', _, normal} -> ok;
        Resp -> Resp
    end.

start_reader(F) ->
    register(reader, spawn_link(fun() ->
        {ok, S} = file:open(F, read),
        loop_reader(S)
    end)),
    ok.

loop_reader(S) ->
    receive
        {read, From} ->
            From ! io:read(S, ''),
            loop_reader(S);
        {quit, From} ->
            From ! file:close(S)
    end.

read_reader() ->
    reader ! {read, self()},
    receive
        Resp -> Resp
    end.

quit_reader() ->
    reader ! {quit, self()},
    receive
        {'EXIT', _, normal} -> ok;
        Resp -> Resp
    end.

writer_reader_test() ->
    File = "data/writer.tmp",
    ok = start_writer(File),
    write_writer(hogehoge),
    write_writer({hoge, hoge}),
    write_writer([{hoge, hoge}, {fuga, fuga}]),
    ok = quit_writer(),
    ok = start_reader(File),
    Msg1 = read_reader(),
    Msg2 = read_reader(),
    Msg3 = read_reader(),
    Msg4 = read_reader(),
    ok = quit_reader(),
    [
        ?_assertEqual({ok, hogehoge}, Msg1),
        ?_assertEqual({ok, {hoge, hoge}}, Msg2),
        ?_assertEqual({ok, [{hoge, hoge}, {fuga, fuga}]}, Msg3),
        ?_assertEqual(eof, Msg4)
    ].

file_consult_test_() ->
    {ok, Actual} = file:consult("data/fuga.dat"),
    Expected = [
                {person, "joe", "armstrong",
                 [
                  {occupation, programmer},
                  {favoriteLanguage, erlang}
                 ]
                },
                {cat,
                 {name, "zorro"},
                 {owner, "joe"}
                }
               ],
    [
     ?_assertEqual(Expected, Actual)
    ].
