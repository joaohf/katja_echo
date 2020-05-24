%%%-------------------------------------------------------------------
%% @hidden
%% @doc Supervisor of Katja Echo application.
%% @end
%%%-------------------------------------------------------------------

-module(katja_echo_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_link/1,
         stop/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_POOL, []).


start_link() ->
    Options = get_options(),
    start_link(Options).


start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Options]).


stop(SupPid) ->
    erlang:exit(SupPid, normal).


%% @private
init([Options]) ->

    katja_echo = ets:new(katja_echo, [set, public, named_table]),

    Options0 = check_options(Options),

    TcpSpec = child_spec(katja_echo_tcp, Options0),
    UpdSpec = child_spec(katja_echo_udp, Options0),

    Pool = proplists:get_value(pool, Options0),

    Children = maybe_add_child(katja_echo_tcp, Pool, TcpSpec, []),
    Children2 = maybe_add_child(katja_echo_udp, Pool, UpdSpec, Children),

    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},

    ChildSpecs = Children2,
    {ok, {SupFlags, ChildSpecs}}.


-spec child_spec(atom(), katja_echo:options()) -> supervisor:child_spec().
child_spec(katja_echo_udp = Mod, Options) ->
  #{id => Mod,
    start => {Mod, start_link, [Options]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [Mod]
  };

child_spec(katja_echo_tcp = Mod, Options) ->
    Port = proplists:get_value(port, Options),
    Ref = {?SERVER, Mod},
    TransportOpts = #{socket_opts => [{port, Port}]},

    ranch:child_spec(Ref, ranch_tcp, TransportOpts, Mod, Options).


-spec maybe_add_child(atom(), [atom()], supervisor:child_spec(),
    [supervisor:child_spec()]) -> [supervisor:child_spec()].

maybe_add_child(Child, Pool, Spec, Children) ->
  case lists:member(Child, Pool) of
    true -> Children;
    false -> [Spec | Children]
  end.


get_options() ->
    F = fun(I, Acc) -> add_option(I, Acc) end,
    lists:foldl(F, [], [I || {I, _} <- katja_echo:default()]).


add_option(Option, Acc) ->
    case application:get_env(katja_echo, Option) of
        {ok, Value} -> [{Option, Value} | Acc];
        undefined -> Acc
    end.


check_options(Options) ->
    Pool = proplists:get_value(pool, Options, katja_echo:default(pool)),
    Callback = proplists:get_value(callback, Options, katja_echo:default(callback)),
    Port = proplists:get_value(port, Options, katja_echo:default(port)),

    [{pool, Pool}, {callback, Callback}, {port, Port}].