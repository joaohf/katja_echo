%%%-------------------------------------------------------------------
%%% File    : katja_echo_SUITE.erl
%%% Author  : João Henrique Ferreira de Freitas
%%% Description : A ct test
%%%
%%% Created : 2020-05-06T20:03:02+00:00
%%%-------------------------------------------------------------------
-module(katja_echo_SUITE).

%% Note: This directive should only be used in test suites.
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

groups() ->
    [
        {start_stop, [], [
            start_and_stop
        ]},
        {udp, [], [
            listen_udp_events
        ]},
        {tcp, [], [
            listen_tcp_events
        ]},
        {query, [], [
            query_events
        ]}
    ].

all() -> 
    [
        {group, start_stop},
        {group, udp},
        {group, tcp},
        {group, query}
    ].


suite() ->
    [{timetrap,{minutes,10}}].


init_per_suite(Config) ->
    Config.


end_per_suite(_Config) ->
    ok.


init_per_group(_GroupName, Config) ->
    {ok, _} = application:ensure_all_started(ranch),
    {ok, _} = application:ensure_all_started(katja),
    Config.


end_per_group(_GroupName, _Config) ->
    ok = application:stop(ranch),
    ok = application:stop(katja),
    ok.


init_per_testcase(_TestCase, Config) ->
    Config.


end_per_testcase(_TestCase, _Config) -> 
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

start_and_stop() -> 
    [].

start_and_stop(_Config) ->
    {ok, _} = application:ensure_all_started(katja_echo),

    ok = application:stop(katja_echo),

    ok.

listen_udp_events() -> 
    [].

listen_udp_events(_Config) -> 
    {ok, Ref, Fun} = reply_events(),

    Event = default_event(),
    Events = default_event(10),

    {ok, Pid} = katja_echo:start_link([{callback, Fun}]),

    ok = katja:send_event(katja_writer, udp, Event),

    check_event(Ref, Event),

    ok = katja:send_events(katja_writer, udp, Events),

    check_event(Ref, Events),

    erlang:exit(Pid, normal),

    ok.


listen_tcp_events() -> 
   [].

listen_tcp_events(_Config) ->
    {ok, Ref, Fun} = reply_events(),

    Event = default_event(),
    Events = default_event(10),

    {ok, Pid} = katja_echo:start_link([{callback, Fun}]),

    ok = katja:send_event(katja_writer, tcp, Event),

    check_event(Ref, Event),

    ok = katja:send_events(katja_writer, tcp, Events),

    check_event(Ref, Events),

    erlang:exit(Pid, normal),

    ok.


query_events() ->
    [].

query_events(_Config) ->
    {ok, Ref, Fun} = reply_events(),

    Event = default_event(),

    {ok, Pid} = katja_echo:start_link([{callback, Fun}]),

    ok = katja:send_event(katja_writer, tcp, Event),

    check_event(Ref, Event),

    {service, Service} = lists:keyfind(service, 1, Event),
    {metric, Metric} = lists:keyfind(metric, 1, Event),

    {ok, [QEvent]} = katja:query_event([{service, Service}]),
    
    {metric, Metric} = lists:keyfind(metric, 1, QEvent),

    erlang:exit(Pid, normal),

    ok.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------

default_event() ->
    [{service, "katja 1"}, {metric, 9001}].

default_event(Count) ->
    F = fun(_Elem, Acc) -> [default_event() | Acc] end,
    lists:foldl(F, [], lists:seq(0, Count - 1)).

events(Events) ->
    ct:pal("events: ~p", [Events]).

reply_events() ->
    Ref = make_ref(),
    Pid = self(),
    F = fun (_Event) -> erlang:send(Pid, {event, Ref, ok}) end,
    {ok, Ref, F}.

check_event(Ref, _Event, Tmo) ->
    receive
        {event, Ref, ok} ->
            ok
    after
        Tmo ->
            ct:fail({event, not_received})
    end.

check_event(Ref, Event) ->
    check_event(Ref, Event, 3000).
