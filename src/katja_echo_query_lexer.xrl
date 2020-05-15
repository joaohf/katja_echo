Definitions.

%STRING      = "([^"]|\\|")*"
STRING      = "[A-Za-z0-9\s]+"
KEYWORD     = [A-Za-z]+
INTEGER     = [0-9]+
FLOAT       = [0-9]+\.[0-9]+((E|e)(\+|\-)?[0-9]+)?
WHITESPACES = [\s\t\n\r]
NIL         = nil|null
TRUE        = true
FALSE       = false
C           = =

%SYMBOL_HEAD = ~('0' .. '9' | '^' | '`' | '\'' | '"' | '#' | '~' | '@' | ':' | '/' | '%' | '(' | ')' | '[' | ']' | '{' | '}' | [ \n\r\t\,] )
%SYMBOL_REST = {SYMBOL_HEAD} | '0'..'9' | '.'

Rules.
{STRING}      : {token, {string, TokenLine, strip(TokenChars,TokenLen)}}.
{KEYWORD}     : {token, {keyword, TokenLine, list_to_atom(TokenChars)}}.
{INTEGER}     : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{FLOAT}       : {token, {number, TokenLine, list_to_float(TokenChars)}}.
{NIL}         : {token, {nil, TokenLine, list_to_atom(TokenChars)}}.
{TRUE}        : {token, {true, TokenLine, list_to_atom(TokenChars)}}.
{FALSE}       : {token, {false, TokenLine, list_to_atom(TokenChars)}}.
{C}           : {token, {comparator,TokenLine,list_to_atom(TokenChars)}}.
{WHITESPACES} : skip_token.

Erlang code.
% Erlang code can be added here

strip(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).
