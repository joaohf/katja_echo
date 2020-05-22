-module(katja_echo_query).

-include_lib("stdlib/include/qlc.hrl").
-include("katja_echo_pb.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([parse/1, query/2]).

-define(LEXER, katja_echo_query_lexer).
-define(PARSER, katja_echo_query_grammar).

parse(<<Bin/binary>>) ->
    parse(binary_to_list(Bin));

parse(String) ->
    {ok, Tokens, _EndLine} = ?LEXER:string(String),
    ?PARSER:parse(Tokens).


query(Tab, P) ->
    Results = qq(Tab, P, []),

    ?debugVal(Results),

    {ok, Results}.

% build match spec

do_query(Tab, MatchConditions) ->
   MS = [{{'_', '$1'}, MatchConditions, ['$1']}],

   % query
   Results = ets:select(Tab, MS),

   {ok, Results}.


qq(_Tab, [], R) ->
    R;

qq(Tab, [{'and', P} | Hs], R) ->

   MatchConditions = [list_to_tuple(['andalso' | q(Tab, P, [])])],

   ?debugFmt("and query: ~p", [MatchConditions]),

   {ok, Results} = do_query(Tab, MatchConditions),

   L = Results ++ R,
   
   qq(Tab, Hs, L);

qq(Tab, [{'or', P} | Hs], R) ->

   MatchConditions = [list_to_tuple(['orelse' | q(Tab, P, [])])],

   ?debugFmt("or query: ~p", [MatchConditions]),

   {ok, Results} = do_query(Tab, MatchConditions),

   L = Results ++ R,
   
   qq(Tab, Hs, L);

qq(Tab, [{field, _, _, _} = P | [] = Hs], R) ->

   MatchConditions = [list_to_tuple(['andalso' | q(Tab, [P], [])])],

   ?debugFmt("and query: ~p", [MatchConditions]),

   {ok, Results} = do_query(Tab, MatchConditions),

   L = Results ++ R,

   qq(Tab, Hs, L);

qq(Tab, [{field, _, _, _} = P | Hs], R) ->

   MatchConditions = [list_to_tuple(['andalso' | q(Tab, [P], [])])],

   ?debugFmt("and query: ~p", [MatchConditions]),

   %{ok, Results} = do_query(Tab, MatchConditions),

   %L = Results ++ R,
   L = MatchConditions ++ R,

   qq(Tab, Hs, L).


q(_Tab, [], R) ->
   R;

% discard R because and is the beggining
q(Tab, [{'and', Q} | Hs], R) ->
   X = q(Tab, Q, R),

  %?debugFmt("zz ~p zz ~p", [X, R]),

   qq(Tab, Hs, [ X ]);

q(Tab, [{'or', Q} | Hs], R) ->
   X = q(Tab, Q, R),

  %?debugFmt("zz ~p zz ~p", [X, R]),

   qq(Tab, Hs, [ X ]);

q(Tab, [{field, time, Op, Time} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 2, '$1'}, {const, Time}} | R]);

q(Tab, [{field, state, Op, State} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 3, '$1'}, {const, State}} | R]);

q(Tab, [{field, service, Op, Service} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 4, '$1'}, {const, Service}} | R]);

q(Tab, [{field, host, Op, Host} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 5, '$1'}, {const, Host}} | R]);

q(Tab, [{field, description, Op, Description} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 6, '$1'}, {const, Description}} | R]);

q(Tab, [{field, tags, Op, Tags} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 7, '$1'}, {const, Tags}} | R]);

q(Tab, [{field, ttl, Op, Ttl} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 8, '$1'}, {const, Ttl}} | R]);

q(Tab, [{field, attributes, Op, Attributes} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 9, '$1'}, {const, Attributes}} | R]);

q(Tab, [{field, time_micros, Op, TimeMicros} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 10, '$1'}, {const, TimeMicros}} | R]);

q(Tab, [{field, metric_sint64, Op, Metric} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 11, '$1'}, {const, Metric}} | R]);

q(Tab, [{field, metric_d, Op, Metric} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 12, '$1'}, {const, Metric}} | R]);

q(Tab, [{field, metric_f, Op, Metric} | Hs], R) ->
   q(Tab, Hs, [{Op, {element, 13, '$1'}, {const, Metric}} | R]).