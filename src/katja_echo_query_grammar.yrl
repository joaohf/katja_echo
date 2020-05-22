Nonterminals predicates predicate primary value.

Terminals 'not' union intersection tagged string field number
comparator nil true false like '(' ')'.

Rootsymbol predicates.

Right 100 'not'.
Left 200 union.
Left 200 intersection.

predicates -> predicate : ['$1'].
predicates -> predicate predicates : ['$1' | '$2'].

predicate -> primary : '$1'.
predicate -> not predicate: [{'not', '$2'}].

%predicate -> predicate union predicate : lists:flatten(['$1', unwrap('$2'), '$3']).
%predicate -> predicate intersection predicate : lists:flatten(['$1', unwrap('$2'), '$3']).

predicate -> predicate union predicate : {unwrap('$2'), ['$1' , '$3']}.
predicate -> predicate intersection predicate : {unwrap('$2'), ['$1' , '$3']}.

primary -> '(' ')'.
primary -> '(' predicate ')' : '$2'.
primary -> field comparator value : {field, unwrap('$1'), unwrap('$2'), unwrap('$3')}.
primary -> field like string : {field, unwrap('$1'), unwrap('$2'), unwrap('$3')}.
primary -> tagged string : {field, unwrap('$1'), unwrap('$2')}.


value -> true : '$1'.
value -> false : '$1'.
value -> nil : '$1'.
value -> number : '$1'.
value -> string : '$1'.
value -> field : '$1'.

Erlang code.

unwrap({_,_,V}) -> V.

