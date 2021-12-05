-module(mars_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(WIDTH, 4).
-define(HEIGHT, 8).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
setup_test_() ->
  [{"The grid is successfully initialized to the size requested",
      ?setup(fun mars_grid_setup/1)}].

comparison_test_() ->
  [{"The rover position is within the grid dimensions",
      ?setup(fun rover_inside_grid_dimensions/1)},
    {"The rover position is outside the grid dimensions",
      ?setup(fun rover_outside_grid_dimensions/1)}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
  {ok, Pid} = mars:start(?WIDTH, ?HEIGHT),
  Pid.

stop(Pid) ->
  ok = mars:stop(Pid).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%%% Setup %%%
mars_grid_setup(Pid) ->
  {ok, {Width, Height}} = mars:get_grid_dimensions(Pid),
  [?_assertMatch(Width, ?WIDTH), ?_assertMatch(Height, ?HEIGHT)].
%%% Setup %%%

%%% Comparison %%%
rover_inside_grid_dimensions(Pid) ->
  % Safe inside grid
  {ok, Result1} = mars:validate_rover_position(Pid, ?WIDTH - (?WIDTH / 2), 
    ?HEIGHT - (?HEIGHT / 2)),
  % Edge case
  {ok, Result2} = mars:validate_rover_position(Pid, 0, 0),
  % Edge case
  {ok, Result3} = mars:validate_rover_position(Pid, ?WIDTH, ?HEIGHT),
  % X position edge
  {ok, Result4} = mars:validate_rover_position(Pid, ?WIDTH, 0),
  % Y position edge
  {ok, Result5} = mars:validate_rover_position(Pid, 0, ?HEIGHT),
  [?_assertMatch(Result1, safe), ?_assertMatch(Result2, safe), 
    ?_assertMatch(Result3, safe), ?_assertMatch(Result4, safe),
    ?_assertMatch(Result5, safe)].

rover_outside_grid_dimensions(Pid) ->
  % Completely outside grid
  {ok, Result1} = mars:validate_rover_position(Pid, ?WIDTH * 2, ?HEIGHT * 2),
  % X position outside grid (+)
  {ok, Result2} = mars:validate_rover_position(Pid, ?WIDTH + 1, ?HEIGHT),
  % X position outside grid (-)
  {ok, Result3} = mars:validate_rover_position(Pid, -1, ?HEIGHT),
  % Y position outside grid (+)
  {ok, Result4} = mars:validate_rover_position(Pid, ?WIDTH, ?HEIGHT + 1),
  % Y position outside grid (-)
  {ok, Result5} = mars:validate_rover_position(Pid, ?WIDTH, -1),
  [?_assertMatch(Result1, lost), ?_assertMatch(Result2, lost),
    ?_assertMatch(Result3, lost), ?_assertMatch(Result4, lost),
    ?_assertMatch(Result5, lost)].
%%% Comparison %%%
