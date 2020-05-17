Definitions.

STRING      = "[%A-Za-z0-9\s\b\t\n\f\r\\\"]*"
KEYWORD     = [A-Za-z\_]*
INTEGER     = (\+|\-)?[0-9]+
FLOAT       = (\+|\-)?[0-9]+\.[0-9]+((E|e)(\+|\-)?[0-9]+)?
WHITESPACES = [\s\t\n\r]
NIL         = nil|null
TRUE        = true
FALSE       = false
C           = (=|>|<|>=|<=|!=)
LIKE        = =~

Rules.
not           : {token,{'not',TokenLine,list_to_atom(TokenChars)}}.
or            : {token,{union,TokenLine,list_to_atom(TokenChars)}}.
and           : {token,{intersection,TokenLine,list_to_atom(TokenChars)}}.
tagged        : {token,{tagged,TokenLine,list_to_atom(TokenChars)}}.
{STRING}      : {token, {string, TokenLine, strip(TokenChars,TokenLen)}}.
{KEYWORD}     : {token, {field, TokenLine, list_to_atom(TokenChars)}}.
{FLOAT}       : {token, {number, TokenLine, list_to_float(TokenChars)}}.
{INTEGER}     : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{NIL}         : {token, {nil, TokenLine, list_to_atom(TokenChars)}}.
{TRUE}        : {token, {true, TokenLine, list_to_atom(TokenChars)}}.
{FALSE}       : {token, {false, TokenLine, list_to_atom(TokenChars)}}.
{LIKE}        : {token, {like,TokenLine,list_to_atom(TokenChars)}}.
{C}           : {token, {comparator,TokenLine,list_to_atom(TokenChars)}}.
[()]         : {token,{list_to_atom(TokenChars),TokenLine}}.
{WHITESPACES} : skip_token.

Erlang code.

atom(TokenChars) ->
    list_to_atom(string:to_lower(TokenChars)).

strip(TokenChars,TokenLen) -> 
    lists:sublist(TokenChars, 2, TokenLen - 2).
