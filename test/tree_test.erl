-module(tree_test).

-include_lib("eunit/include/eunit.hrl").

create_new_tree_test() ->
    T = tree:insert("Hoge", "hoge@some-email", tree:empty()),
    ?assertEqual(
        {node, {
            "Hoge",
            "hoge@some-email",
            {node, 'nil'},
            {node, 'nil'}}}, T).

insert_to_tree_test() ->
    T1 = tree:insert("Hoge", "hoge@some-email", tree:empty()),
    T2 = tree:insert("Fuga", "fuga@some-email", T1),
    T3 = tree:insert("ZZZZ", "zzzz@some-email", T2),
    T4 = tree:insert("YYYY", "yyyy@some-email", T3),
    ?assertEqual(
        {node, {
            "Hoge",
            "hoge@some-email",
            {node, {
                "Fuga",
                "fuga@some-email",
                {node, 'nil'},
                {node, 'nil'}}},
            {node, 'nil'}}}, T2),
    ?assertEqual(
        {node, {
            "Hoge",
            "hoge@some-email",
            {node, {
                "Fuga",
                "fuga@some-email",
                {node, 'nil'},
                {node, 'nil'}}},
            {node, {
                "ZZZZ",
                "zzzz@some-email",
                {node, 'nil'},
                {node, 'nil'}}}}}, T3),
    ?assertEqual(
        {node, {
            "Hoge",
            "hoge@some-email",
            {node, {
                "Fuga",
                "fuga@some-email",
                {node, 'nil'},
                {node, 'nil'}}},
            {node, {
                "ZZZZ",
                "zzzz@some-email",
                {node, {
                    "YYYY",
                    "yyyy@some-email",
                    {node, 'nil'},
                    {node, 'nil'}}},
                {node, 'nil'}}}}}, T4).

insert_overwrites_tree_test() ->
    T1 = tree:insert("Hoge", "hoge@some-email", tree:empty()),
    T2 = tree:insert("Fuga", "fuga@some-email", T1),
    T3 = tree:insert("Fuga", "fuga2@some-email", T2),
    ?assertEqual(
        {node, {
            "Hoge",
            "hoge@some-email",
            {node, {
                "Fuga",
                "fuga2@some-email",
                {node, 'nil'},
                {node, 'nil'}}},
            {node, 'nil'}}}, T3).

lookup_a_tree_test() ->
    T1 = tree:insert("Hoge", "hoge@some-email", tree:empty()),
    T2 = tree:insert("Fuga", "fuga@some-email", T1),
    T3 = tree:insert("ZZZZ", "zzzz@some-email", T2),
    T4 = tree:insert("YYYY", "yyyy@some-email", T3),
    T5 = tree:insert("YYYY", "yyy2@some-email", T4),
    ?assertEqual({ok, "hoge@some-email"}, tree:lookup("Hoge", T5)),
    ?assertEqual({ok, "fuga@some-email"}, tree:lookup("Fuga", T5)),
    ?assertEqual({ok, "zzzz@some-email"}, tree:lookup("ZZZZ", T5)),
    ?assertEqual({ok, "yyy2@some-email"}, tree:lookup("YYYY", T5)).
