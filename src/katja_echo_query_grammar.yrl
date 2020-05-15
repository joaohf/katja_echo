Header "%% Copyright (C)"
"%% @private"
"%% @Author John".

Nonterminals E.
Terminals number '+' '*' '(' ')'.
Rootsymbol E.

Left 100 '+'.
Left 200 '*'.

E -> E '+' E   : {'$2', '$1', '$3'}.
E -> E '*' E   : {'$2', '$1', '$3'}.
E -> '(' E ')' : '$2'.
E -> number    : '$1'.

Erlang code.