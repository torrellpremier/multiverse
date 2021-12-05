%%%-------------------------------------------------------------------
%% @doc multiverse_mars_rover public API
%% @end
%%%-------------------------------------------------------------------

-module(multiverse_mars_rover_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    multiverse_mars_rover_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
