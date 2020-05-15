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

-callback events(Events :: list()) -> ok.

-type katja_echo_options() :: [katja_echo_option()].
-type katja_echo_option()  ::
      {pool, [atom()]}
    | {port, non_neg_integer()}
    | {callback, module()}.
-export_type([katja_echo_option/0, katja_echo_options/0]).

-include_lib("katja_echo_pb.hrl").


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
%% Process all `Events'
%% @end
%%---------------------------------------------------------------------

-spec events(Callback :: module() | fun(), Events :: list()) -> ok.

events(Module, Events) when is_atom(Module) ->
    catch Module:events(Events),
    ok;

events(Callback, Events) when is_function(Callback, 1) ->
    catch Callback(Events),
    ok.


%%---------------------------------------------------------------------
%% @private
%% @doc
%% Process a query
%% @end
%%---------------------------------------------------------------------

-spec query(Callback :: module() | fun(), Events :: list()) -> {ok, list()}.

query(Module, Query) when is_atom(Module) ->
    {ok, Query1} = do_query(Query),
    {ok, _Results} = Module:query(Query1);

query(Callback, Query) when is_function(Callback, 1) ->
    {ok, Query1} = do_query(Query),
    {ok, _Results} = Callback:query(Query1).


do_query(_Query) ->
    {ok, []}.

%%---------------------------------------------------------------------
%% @doc
%% Default function to process events
%% @end
%%---------------------------------------------------------------------

-spec events(Events :: list()) -> ok.

events(_Events) ->    
    ok.


%%---------------------------------------------------------------------
%% @doc
%% Default function to process query
%% @end
%%---------------------------------------------------------------------

-spec query(Query :: any()) -> {ok, list()}.

query(_Query) ->
    {ok, []}.


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