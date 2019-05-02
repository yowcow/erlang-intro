-module(xmerl_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

render_test_() ->
    Cases = [
        {
            "simple proplist",
            [
                {
                    items,
                    [{count, 2}],
                    [
                        {
                            fruit,
                            [{color, "red"}, {produced, "2019-05-02T16:06:53+09:00"}],
                            [
                                {name, ["apple"]}
                            ]
                        },
                        {
                            fruit,
                            [{color, "orange"}, {produced, "2019-05-02T16:06:53+09:00"}],
                            [
                                {name, ["orange"]}
                            ]
                        }
                    ]
                }
            ],
            "<?xml version=\"1.0\"?>"
            "<items count=\"2\">"
            "<fruit color=\"red\" produced=\"2019-05-02T16:06:53+09:00\"><name>apple</name></fruit>"
            "<fruit color=\"orange\" produced=\"2019-05-02T16:06:53+09:00\"><name>orange</name></fruit>"
            "</items>"
        },
        {
            "xml elements",
            [
                #xmlElement{
                    name = items,
                    attributes = [{count, 2}],
                    content = [
                        #xmlElement{
                            name = fruit,
                            attributes = [{color, "red"}, {produced, "2019-05-02T16:06:53+09:00"}],
                            content = [
                                #xmlElement{
                                    name = name,
                                    content = [
                                        #xmlText{
                                            value = "apple"
                                        }
                                    ]
                                }
                            ]
                        },
                        #xmlElement{
                            name = fruit,
                            attributes = [{color, "orange"}, {produced, "2019-05-02T16:06:53+09:00"}],
                            content = [
                                #xmlElement{
                                    name = name,
                                    content = [
                                        #xmlText{
                                            value = "orange"
                                        }
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ],
            "<?xml version=\"1.0\"?>"
            "<items count=\"2\">"
            "<fruit color=\"red\" produced=\"2019-05-02T16:06:53+09:00\"><name>apple</name></fruit>"
            "<fruit color=\"orange\" produced=\"2019-05-02T16:06:53+09:00\"><name>orange</name></fruit>"
            "</items>"
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = lists:flatten(xmerl:export_simple(Input, xmerl_xml)),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).
