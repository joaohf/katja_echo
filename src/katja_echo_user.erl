%%%-------------------------------------------------------------------
%% @doc Module with fallback katja_echo implementation.
%% @end
%%%-------------------------------------------------------------------

-module(katja_echo_user).

-behaviour(katja_echo).

-export([events/1, query/1]).

%%---------------------------------------------------------------------
%% @doc
%% Fallback function to process events. If the user have not been defined
%% a callback to receive events, this function will be called.
%% @end
%%---------------------------------------------------------------------

-spec events(Events :: katja_echo:events()) -> ok.

events(_Events) ->
    ok.


%%---------------------------------------------------------------------
%% @doc
%% Fallback function to process queried events. If the user have not been defined
%% a callback to receive events, this function will be called.
%% @end
%%---------------------------------------------------------------------

-spec query(Query :: any()) -> ok.

query(_Query) ->
    ok.