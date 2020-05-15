-module(katja_echo_query_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LEXER, katja_echo_query_lexer).
-define(PARSER, katja_echo_query_grammar).

simple_query_test() ->
    String = "service = \"katja 1\"",
    String0 = "host = 1 and state = 2",
    String1 = "not ((host = 1 or host = 2) and host = 3)",

    {ok,Tokens,EndLine} = ?LEXER:string(String),
    
    {ok,Tokens1,EndLine1} = ?LEXER:string(String1),

    ?debugFmt("~p", [Tokens]),
    ?debugFmt("~p", [Tokens1]),

    K = ?PARSER:parse(Tokens),

    ?debugFmt("~p", [K]),
    ok.

aquery_lexer_test_() ->
    String = "service = \"katja 1\"",
    ?_test({ok,_Tokens,_EndLine} = ?LEXER:string(String)).

query_lexer_test_() ->
    L = [
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
       "state = \"辻斬\"",

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

       "not ((host = 1 or host = 2) and host = 3)",

       "service = \"katja 1\""],

    query_lexer_gen(L).

query_lexer_gen([]) ->
    {generator, fun() -> [] end};

query_lexer_gen([H|Rest]) ->
    {generator,
        fun () ->
            [?_assertMatch({ok, _, _}, ?LEXER:string(H))| query_lexer_gen(Rest)]
        end}.