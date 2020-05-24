%%%-------------------------------------------------------------------
%% @hidden
%% @doc Module for the Katja Echo application behaviour.
%% @end
%%%-------------------------------------------------------------------

-module(katja_echo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    katja_echo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
