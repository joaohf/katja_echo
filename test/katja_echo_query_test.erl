-module(katja_echo_query_test).

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