Nonterminals predicate primary value.

Terminals 'not' union intersection tagged string field number
comparator nil true false like '(' ')'.

Rootsymbol predicate.

Right 100 'not'.
Left 200 union.
Left 200 intersection.

predicate -> primary : '$1'.
predicate -> not predicate: {'not', '$2'}.
predicate -> primary union primary : {union, '$1', '$3'}.
predicate -> predicate union primary : {union, '$1', '$3'}.

predicate -> primary intersection primary : {intersection, '$1', '$3'}.
predicate -> predicate intersection primary : {intersection, '$1', '$3'}.

primary -> '(' ')'.
primary -> '(' predicate ')' : {primary, '$2'}.
primary -> field comparator value : {predicate, {field, unwrap('$1')}, unwrap('$2'), unwrap('$3')}.
primary -> field like string : {predicate, {field, unwrap('$1')}, unwrap('$2'), unwrap('$3')}.
primary -> tagged string : {predicate, {field, tagged, unwrap('$2')}}.


value -> true : '$1'.
value -> false : '$1'.
value -> nil : '$1'.
value -> number : '$1'.
value -> string : '$1'.
value -> field : '$1'.

Erlang code.

unwrap({_,_,V}) -> V.