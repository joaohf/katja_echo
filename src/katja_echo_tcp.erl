%%%-------------------------------------------------------------------
%% @hidden
%% @doc Module responsible for receiving TCP packets. Also implements
%% the ranch_tcp protocol.
%% @end
%%%-------------------------------------------------------------------

-module(katja_echo_tcp).

-behaviour(gen_statem).

-include_lib("katja_echo_pb.hrl").

-export([start_link/1,
         stop/1]).

-export([callback_mode/0,
         init/1,
         terminate/3]).

-export([idle/3,
         connected/3]).

-record(state, {callback = undefined :: module() | fun(),
                lsocket = undefined :: undefined | gen_tcp:socket(),
                socket = undefined :: undefined | gen_tcp:socket(),
                data = <<>> :: binary(),
                errors = #{} :: map()}).

-type state() :: #state{}.

%%---------------------------------------------------------------------
%% @doc
%% Start process to handle the connection.
%% @end
%%---------------------------------------------------------------------

-spec start_link(Socket :: gen_tcp:socket()) -> {ok, pid()}.

start_link(SocketAndOptions) ->
    gen_statem:start_link(?MODULE, SocketAndOptions, []).


%%---------------------------------------------------------------------
%% @doc
%% Stop the server.
%% @end
%%---------------------------------------------------------------------

-spec stop(gen_statem:server_ref()) -> ok.

stop(Pid) ->
    gen_statem:stop(Pid).


%%---------------------------------------------------------------------
%% @doc
%% Returns the callback mode.
%% @end
%%---------------------------------------------------------------------
callback_mode() -> [state_functions, state_enter].


%%---------------------------------------------------------------------
%% @doc
%% Init function
%% @end
%%---------------------------------------------------------------------

-spec init({gen_tcp:socket(), katja_echo:katja_echo_options()}) -> any().

init({LSocket, Opts}) ->
    Cbk = katja_echo:callback(Opts),

    Data = #state{callback = Cbk, lsocket = LSocket},

    Actions = [{next_event, internal, establish}],
    {ok, idle, Data, Actions}.


terminate(_Reason, _State, _Data) ->
    ok.


idle(enter, idle, _Data) ->
    keep_state_and_data;

idle(internal, establish, #state{lsocket = LSocket} = Data) ->
    {ok, AcceptSocket} = gen_tcp:accept(LSocket),

    NData = Data#state{socket = AcceptSocket},

    {next_state, connected, NData}.


connected(enter, idle, #state{socket = Socket}) ->
    ok = inet:setopts(Socket, [{active, once}]),

    keep_state_and_data;

connected(info, {tcp, Socket, Packet}, #state{socket = Socket} = Data) ->
    {ok, NData} = process_packet(Data, Packet),

    ok = inet:setopts(Socket, [{active, once}]),

    {keep_state, NData};

connected(info, {tcp_closed, Socket}, #state{socket = Socket}) ->
    {stop, normal};

connected(info, {tcp_error, Socket, _Reason}, #state{socket = Socket} = Data) ->
    ok = gen_tcp:close(Socket),
    {stop, normal, Data#state{socket = undefined}}.


-spec process_packet(state(), binary()) -> ok.

process_packet(#state{socket = Socket, callback = Cbk, errors = Errors, data = Acc} = State,
    Packet) ->
    BinMsg2 = <<Acc/binary, Packet/binary>>,
    case katja_echo:decode(tcp, BinMsg2) of
        {ok, #riemannpb_msg{ok=true, events = [], states = [],
            query = #riemannpb_query{string = Query}}} ->

            {ok, Results} = katja_echo:query(Cbk, Query),

            send_riemann_reply(Socket, Results),

        {ok, State#state{data = <<>>}};
            {ok, #riemannpb_msg{ok=true, events = Events}} ->
            send_riemann_reply(Socket),

            ok = katja_echo:events(Cbk, Events),

        {ok, State#state{data = <<>>}};
            {error, #riemannpb_msg{ok=false, error=Reason}} ->
            Errors0 = maps:update_with(Reason, fun katja_echo:incr/1, Errors),
            {ok, State#state{data = <<>>, errors = Errors0}};
        {error, Reason} ->
            Errors0 = maps:update_with(Reason, fun katja_echo:incr/1, Errors),
            {ok, State#state{data = BinMsg2, errors = Errors0}}
    end.


send_riemann_reply(Socket) ->
    Msg = katja_pb:encode_riemannpb_msg(#riemannpb_msg{ok=true}),
    BinMsg = iolist_to_binary(Msg),
    MsgSize = byte_size(BinMsg),
    ok = gen_tcp:send(Socket, <<MsgSize:32/integer-big, BinMsg/binary>>).


send_riemann_reply(Socket, Msg) ->
    Msg1 = katja_pb:encode_riemannpb_msg(#riemannpb_msg{ok=true, events = Msg}),
    BinMsg = iolist_to_binary(Msg1),
    MsgSize = byte_size(BinMsg),
    ok = gen_tcp:send(Socket, <<MsgSize:32/integer-big, BinMsg/binary>>).