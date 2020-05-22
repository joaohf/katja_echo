-module(katja_echo).

-export([start_link/0,
         start_link/1,
         default/0,
         default/1,
         callback/1,
         events/2,
         events/1,
         query/2,
         query/1,
         decode/2,
         incr/1]).

-callback events(events()) -> ok.
-callback query(events()) -> ok.

-include("katja_echo_pb.hrl").

-type event() :: #riemannpb_event{}.
-type state() :: #riemannpb_state{}.
-type events() :: [event() | state()].
-type_export([event/0, state/0]).

-type katja_echo_options() :: [katja_echo_option()].
-type katja_echo_option()  ::
      {pool, [atom()]}
    | {port, non_neg_integer()}
    | {callback, module()}.
-export_type([katja_echo_option/0, katja_echo_options/0]).

-define(TAB, ?MODULE).


start_link() ->
    katja_echo_sup:start_link().


start_link(Options) when is_list(Options) ->
    katja_echo_sup:start_link(Options).


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Provide default value of a given option.
%% @end
%%---------------------------------------------------------------------

-spec default() -> katja_echo_options().

default() ->
    [{pool, []}, {callback, katja_echo}, {port, 5555}].


default(Option) ->
    proplists:get_value(Option, default()).


callback(Opts) ->
    case lists:keyfind(callback, 1, Opts) of
        {_, Mod} when is_atom(Mod) ->
            Mod;
        {_, Fun} when is_function(Fun, 1) ->
            Fun;
        _ ->
            default(callback)
    end.

%%---------------------------------------------------------------------
%% @private
%% @doc
%% Process all `Events'. Write events in ETS table and call the user
%% callback for further processing.
%% @end
%%---------------------------------------------------------------------

-spec events(Callback :: module() | fun(), Events :: list()) -> ok.

events(Module, Events) when is_atom(Module) ->
    ok = do_events(Events),
    catch Module:events(Events),
    ok;

events(Callback, Events) when is_function(Callback, 1) ->
    ok = do_events(Events),
    catch Callback(Events),
    ok.


do_events(Events) when is_list(Events) ->
    _ = [insert_event(Event) || Event <- Events],
    ok.

insert_event(#riemannpb_event{host = H, service = S} = E) ->
    ets:insert(?TAB, {{H, S}, E});

insert_event(#riemannpb_state{host = H, service = S} = E) ->
    ets:insert(?TAB, {{H, S}, E}).


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Process a query parsing and fetching data from ETS table. Call the
%% user callback for further processing.
%% @end
%%---------------------------------------------------------------------

-spec query(Callback :: module() | fun(), Events :: list()) -> {ok, list()}.

query(Module, Query) when is_atom(Module) ->
    {ok, Results} = do_query(Query),
    catch (Module:query(Results)),
    {ok, Results};

query(Callback, Query) when is_function(Callback, 1) ->
    {ok, Results} = do_query(Query),
    catch (Callback(Results)),
    {ok, Results}.


do_query({ok, ParseTree}) ->
    katja_echo_query:query(?TAB, ParseTree);
    
do_query({error, {_LineNumber, Module, Message}}) ->
    {error, {"syntax error", Module:format_error(Message)}};

do_query(Query) ->
    do_query(katja_echo_query:parse(Query)).

%%---------------------------------------------------------------------
%% @doc
%% Default function to process events
%% @end
%%---------------------------------------------------------------------

-spec events(Events :: events()) -> ok.

events(_Events) ->
    ok.


%%---------------------------------------------------------------------
%% @doc
%% Default function to process query
%% @end
%%---------------------------------------------------------------------

-spec query(Query :: any()) -> {ok, list(event() | state())}.

query(_Query) ->
    ok.


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Decode riemann protocol
%% @end
%%---------------------------------------------------------------------

decode(udp, Msg) when is_binary(Msg) ->
    try katja_echo_pb:decode_riemannpb_msg(Msg) of
        R ->
            {ok, R#riemannpb_msg{ok=true}}
    catch
        error:_Reason ->
            {eror, #riemannpb_msg{ok=false, error="Response could not be decoded"}}
    end;

decode(tcp, <<MsgSize:32/integer-big, Msg/binary>>) when MsgSize > byte_size(Msg) ->
    {error, too_short};

decode(tcp, <<MsgSize:32/integer-big, Msg/binary>>) ->
    case Msg of
        <<Msg2:MsgSize/binary, _Rest/binary>> ->
            R = katja_echo_pb:decode_riemannpb_msg(Msg2),
            {ok, R#riemannpb_msg{ok=true}};
        _ ->
            {error, #riemannpb_msg{ok=false, error="Response could not be decoded"}}
    end;

decode(tcp, _) ->
    {error, invalid_packet}.


incr(V) -> V + 1.