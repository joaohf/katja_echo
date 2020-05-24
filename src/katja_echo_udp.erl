%%%-------------------------------------------------------------------
%% @hidden
%% @doc Module responsible for receiving UDP packets.
%% @end
%%%-------------------------------------------------------------------

-module(katja_echo_udp).

-behaviour(gen_server).

%% API
-export([start_link/1,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          socket = undefined,
          callback = undefined,
          errors = #{} :: map()
         }).

-type state() :: #state{}.

-include_lib("katja_echo_pb.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------

-spec start_link(Opts :: katja_echo:katja_echo_options()) -> {ok, pid()}.

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%% @end
%%--------------------------------------------------------------------

-spec stop(Pid :: pid()) -> ok.

stop(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------

-spec init(Args :: list()) -> {ok, State :: state()}.

init([Opts]) ->
    Port = proplists:get_value(port, Opts),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),
    Cbk = katja_echo:callback(Opts),
    {ok, #state{socket = Socket, callback = Cbk}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------

-spec handle_call(_Request, _From, State :: state()) -> {reply, ok, State :: state()}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------

-spec handle_cast(_Msg, State :: state()) -> {noreply, State :: state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------

-spec handle_info(_Info, State :: state()) -> {noreply, State :: state()}.

handle_info({udp, _Socket, _IP, _InPortNo, Packet},
    #state{callback = Cbk, errors = Errors} = State) ->

    NState =
    case katja_echo:decode(udp, Packet) of
        {ok, #riemannpb_msg{ok=true, events = Events}} ->
            ok = katja_echo:events(Cbk, Events),
            State;
        {error, #riemannpb_msg{ok=false, error=Reason}} ->
            Errors0 = maps:update_with(Reason, fun katja_echo:incr/1, 1, Errors),
            State#state{errors = Errors0};
        {error, Reason} ->
            Errors0 = maps:update_with(Reason, fun katja_echo:incr/1, 1, Errors),
            State#state{errors = Errors0}
    end,
    {noreply, NState};

handle_info(_Info, State) ->
    %lager:error("Unknown udp message: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------

-spec terminate(_Reason, _State) -> ok.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------

-spec code_change(_OldVsn, State, _Extra) -> {ok, State}.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
