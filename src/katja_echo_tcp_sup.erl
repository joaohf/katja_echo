%%%-------------------------------------------------------------------
%% @hidden
%% @doc Module responsible for supervising TCP workers.
%% @end
%%%-------------------------------------------------------------------
-module(katja_echo_tcp_sup).
-behaviour(supervisor).

-export([start_link/1, start_socket/0]).
-export([init/1]).

-define(NUM_WORKERS, 20).

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).


%% @private
init([Options]) ->
    Port = proplists:get_value(port, Options),
    SocketOpts = [{reuseaddr, true}, {active, false}, binary],
    {ok, ListenSocket} = gen_tcp:listen(Port, SocketOpts),

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 10},

    Children = [child_spec(katja_echo_tcp, {ListenSocket, Options})],

    spawn_link(fun empty_listeners/0),

    {ok, {SupFlags, Children}}.


-spec child_spec(atom(), gen_tcp:socket()) -> supervisor:child_spec().
child_spec(Mod, Options) ->
  #{id => Mod,
    start => {Mod, start_link, [Options]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [Mod]
  }.


start_socket() ->
    supervisor:start_child(?MODULE, []).


empty_listeners() ->
    [start_socket() || _ <- lists:seq(1, ?NUM_WORKERS)],
    ok.