-module(katja_echo_query).

-export([parse/1]).

-define(LEXER, katja_echo_query_lexer).
-define(PARSER, katja_echo_query_grammar).

parse(String) ->
    {ok,Tokens, _EndLine} = ?LEXER:string(String),
    {ok, _ParseTree} = ?PARSER:parse(Tokens).
