%%%-------------------------------------------------------------------
%% @doc Main Katja Echo API.
%% @end
%%%-------------------------------------------------------------------

-module(katja_echo).

-export([start_link/0,
         start_link/1,
         default/0,
         default/1,
         callback/1,
         events/2,
         query/2,
         decode/2,
         incr/1]).

-callback events(Events :: events()) -> ok.

-callback query(Events :: events()) -> ok.

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

%%---------------------------------------------------------------------
%% @doc
%% Start a supervisor tree to handle udp and tcp connections.
%% @see start_link/1
%% @end
%%---------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.

start_link() ->
    katja_echo_sup:start_link().


%%---------------------------------------------------------------------
%% @doc
%% Start a supervisor tree to handle udp and tcp connections.
%% @param Options is a list of config values.
%% @end
%%---------------------------------------------------------------------

-spec start_link(Options :: katja_echo_options()) -> {ok, pid()}.

start_link(Options) when is_list(Options) ->
    katja_echo_sup:start_link(Options).


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Returns the default configuration.
%% @end
%%---------------------------------------------------------------------

-spec default() -> katja_echo_options().

default() ->
    [{pool, []}, {callback, katja_echo_user}, {port, 5555}].


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Provides default value for a given option.
%% @end
%%---------------------------------------------------------------------

-spec default(Options :: atom()) -> term().

default(Option) ->
    proplists:get_value(Option, default()).


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Returns the current callback configurated.
%% @end
%%---------------------------------------------------------------------

-spec callback(Options :: katja_echo_options()) -> module() | function().

callback(Options) ->
    case lists:keyfind(callback, 1, Options) of
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
%% Process all `Events'. Writing each event into ETS table. Calls the user
%% callback for further processing.
%% @end
%%---------------------------------------------------------------------

-spec events(Callback :: module() | fun((Events :: events()) -> ok),
    Events :: events()) -> ok.

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
%% @doc
%% Process a query scanning, parsing and fetching data from ETS table.
%% Call the user callback for further processing. If `Callback' is
%% undefined, no user callback will be called.
%% @end
%%---------------------------------------------------------------------

-spec query(Callback :: undefined | module() | fun((events()) -> ok), Events :: events()) ->
    {ok, events()}.

query(undefined, Query) ->
    do_query(Query);

query(Module, Query) when is_atom(Module) ->
    Results = check_results(do_query(Query)),
    catch (Module:query(Results)),
    {ok, Results};

query(Callback, Query) when is_function(Callback, 1) ->
    Results = check_results(do_query(Query)),
    catch (Callback(Results)),
    {ok, Results}.


check_results({ok, Results}) ->
    Results;

check_results({error, _Reason} = E) ->
    E.


do_query({ok, ParseTree}) ->
    katja_echo_query:query(?TAB, ParseTree);

do_query({error, {_LineNumber, Module, Message}}) ->
    {error, {"syntax error", Module:format_error(Message)}};

do_query(Query) ->
    do_query(katja_echo_query:parse(Query)).


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Decode riemann protocol. This functions works decoding a whole buffer
%% that must have a valid riemann packet.
%% @end
%%---------------------------------------------------------------------

-spec decode(tcp | udp, Msg :: binary()) ->
    {ok, katja_echo_pb:riemannpb_msg()} | {error, katja_echo_pb:riemannpb_msg()} |
    {error, too_short} | {error, invalid_packet}.

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


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Helper function that increments a integer value.
%% @end
%%---------------------------------------------------------------------
incr(V) -> V + 1.