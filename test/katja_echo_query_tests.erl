-module(katja_echo_query_tests).

-include_lib("eunit/include/eunit.hrl").

-include("katja_echo_pb.hrl").

-define(LEXER, katja_echo_query_lexer).
-define(PARSER, katja_echo_query_grammar).

simple_query_test() ->
    %String = "service = \"s1\" and host =~ \"h1\" and (state = 1 or time != 2) and ttl = 10 and metric = 10 and tagged \"cat\"",
    %String = "service = \"s1\" and host =~ \"h1\" and (state = 1 and time != 2)",
    %String = "state = 3",
    String = "service = \"s1 line_up\"",

    {ok, ParseTree} = katja_echo_query:parse(String),
?debugVal(ParseTree),
    Tab = ets:new(katja_echo, [set, private]),

    Events = [
        {{<<"h1">>, <<"s1 line_up">>},
         #riemannpb_event{host = <<"h1">>, service = <<"s1 line_up">>, state = 1, metric_d = 1, metric_f = 1}},
        {{<<"h2">>, <<"s2">>},
         #riemannpb_event{host = <<"h2">>, service = <<"s2">>, state = 1, metric_d = 2, metric_f = 2}},
        {{<<"h3">>, <<"s1">>},
         #riemannpb_event{host = <<"h3">>, service = <<"s1">>, state = 3, metric_d = 3, metric_f = 3}}
    ],

    true = ets:insert(Tab, Events),

    {ok, Results} = katja_echo_query:query(Tab, ParseTree),

    ?debugFmt("spec: ~p results: ~p", [ParseTree, Results]),
    ok.

query_lexer_test_() ->
    Q = queries(),
    query_lexer_gen(Q).

query_lexer_gen([]) ->
    {generator, fun() -> [] end};

query_lexer_gen([H|Hs]) ->
    {generator,
        fun () ->
            [?_assertMatch({ok, _, _}, ?LEXER:string(H)) | query_lexer_gen(Hs)]
        end}.


query_parse_test_() ->
    Q = queries(),
    query_parse_gen(Q).

query_parse_gen([]) ->
    {generator, fun() -> [] end};

query_parse_gen([H|Hs]) ->
    {generator,
        fun () ->
            [?_assertMatch({ok, _}, katja_echo_query:parse(H)) | query_lexer_gen(Hs)]
        end}.


queries() ->
    [
       % Fields
       "state = true",
       "host = true",
       "service = true",
       "description = true",
       "metric_f = true",
       "metric = true",
       "time = true",
       "ttl = 64",

       % Literals
       "true",
       "false",
       "nil",
       "null",

       % Integers
       "state = 0",
       "state = 1",
       "state = -1",

       % Floats
       "state = 0.0",
       "state = 1.5",
       "state = -1.5",
       "state = 1e5",
       "state = 1E5",
       "state = -1.2e-5",

       % Strings
       "state = \"\"",
       "state = \"foo\"",
       "state = \"\\b\\t\\n\\f\\r\"",
       "state = \" \\\" \\\\ \"",
       %"state = \"辻斬\"",
       "state = \"katja 1\"",

       % Simple predicates
       "state = 2",
       "state > 2",
       "state < 2",
       "state >= 2",
       "state <= 2",
       "state != 2",

       "state =~ \"%foo%\"",

       % Tags
       "tagged \"cat\"",

       % Boolean operators
       "not host = 1",
       "host = 1 and state = 2",
       "host = 1 or state = 2",

       % Grouping
       "(host = 1)",
       "((host = 1))",

       % Precedence
       "not host = 1 and host = 2",

       "not host = 1 or host = 2 and host = 3",

       "not ((host = 1 or host = 2) and host = 3)"
    ].