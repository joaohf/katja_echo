%%%-------------------------------------------------------------------
%% @hidden
%% @doc Module responsible for receiving TCP packets. Also implements
%% the ranch_tcp protocol.
%% @end
%%%-------------------------------------------------------------------

-module(katja_echo_tcp).
-behaviour(ranch_protocol).

-include_lib("katja_echo_pb.hrl").

-export([start_link/3]).
-export([init/3]).

-record(state, {callback = undefined :: module() | fun(),
                errors = #{} :: map()}).

-type state() :: #state{}.

%%---------------------------------------------------------------------
%% @doc
%% Start the connection handler
%% @end
%%---------------------------------------------------------------------

-spec start_link(Ref :: ranch:ref(), Transport :: module(),
    Opts :: katja_echo:katja_echo_options()) -> {ok, pid()}.

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

%%---------------------------------------------------------------------
%% @doc
%% Init function
%% @end
%%---------------------------------------------------------------------

-spec init(Ref :: ranch:ref(), Transport :: module(),
    Opts :: katja_echo:katja_echo_options()) -> any().

init(Ref, Transport, Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    Cbk = katja_echo:callback(Opts),
    loop(Socket, Transport, <<>>, #state{callback = Cbk}).


%%---------------------------------------------------------------------
%% @doc
%% Init function
%% @end
%%---------------------------------------------------------------------

-spec loop(Socket :: any(), Transport :: module(), Acc :: binary(), state()) -> ok.

loop(Socket, Transport, Acc, #state{callback = Cbk, errors = Errors} = State) ->
    {OK, Closed, Error, _Passive} = Transport:messages(),
    ok = Transport:setopts(Socket, [{active, once}]),
    receive
        {OK, Socket, Data} ->
            BinMsg2 = <<Acc/binary, Data/binary>>,
            case katja_echo:decode(tcp, BinMsg2) of
                {ok, #riemannpb_msg{ok=true, events = [], states = [],
                 query = #riemannpb_query{string = Query}}} ->

                    {ok, Results} = katja_echo:query(Cbk, Query),

                    send_riemann_reply(Socket, Transport, Results),

                    loop(Socket, Transport, <<>>, State);
                {ok, #riemannpb_msg{ok=true, events = Events}} ->
                    send_riemann_reply(Socket, Transport),

                    ok = katja_echo:events(Cbk, Events),

                    loop(Socket, Transport, <<>>, State);
                {error, #riemannpb_msg{ok=false, error=Reason}} ->
                    Errors0 = maps:update_with(Reason, fun katja_echo:incr/1, Errors),
                    loop(Socket, Transport, <<>>, State#state{errors = Errors0});
                {error, Reason} ->
                    Errors0 = maps:update_with(Reason, fun katja_echo:incr/1, Errors),
                    loop(Socket, Transport, BinMsg2, State#state{errors = Errors0})
            end;
        {Closed, Socket} ->
            ok;
        {Error, Socket, _Reason} ->
            ok = Transport:close(Socket)
    end.


send_riemann_reply(Socket, Transport) ->
    Msg = katja_pb:encode_riemannpb_msg(#riemannpb_msg{ok=true}),
    BinMsg = iolist_to_binary(Msg),
    MsgSize = byte_size(BinMsg),
    ok = Transport:send(Socket, <<MsgSize:32/integer-big, BinMsg/binary>>).


send_riemann_reply(Socket, Transport, Msg) ->
    Msg1 = katja_pb:encode_riemannpb_msg(#riemannpb_msg{ok=true, events = Msg}),
    BinMsg = iolist_to_binary(Msg1),
    MsgSize = byte_size(BinMsg),
    ok = Transport:send(Socket, <<MsgSize:32/integer-big, BinMsg/binary>>).