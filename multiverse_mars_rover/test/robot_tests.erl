-module(robot_tests).
-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(XPOS, 2).
-define(YPOS, 3).
-define(ORIENTATION, "E").
-define(MARS_WIDTH, 4).
-define(MARS_HEIGHT, 8).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
setup_test_() ->
  [{"The robot is successfully initialized to the position",
      ?setup(fun robot_setup/1)}].

directions_test_() -> 
  [{"The robot can take valid directions and end in the correct positions",
      ?setup(fun robot_valid_directions/1)},
    {"The robot can take invalid directions and hold the last known correct position",
      ?setup(fun robot_invalid_directions/1)}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start() ->
  {ok, RobotPid} = robot:start(?XPOS, ?YPOS, ?ORIENTATION),
  {ok, MarsPid} = mars:start(?MARS_WIDTH, ?MARS_HEIGHT),
  {RobotPid, MarsPid}.

stop({RobotPid, MarsPid}) ->
  ok = mars:stop(MarsPid),
  ok = robot:stop(RobotPid).

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

%%% Setup %%%
robot_setup({Pid, _}) ->
  {ok, {XPos, YPos, Orientation}} = robot:get_location(Pid),
  [?_assertMatch(XPos, ?XPOS), ?_assertMatch(YPos, ?YPOS),
    ?_assertMatch(Orientation, ?ORIENTATION)].
%%% Setup %%%

%%% Directions %%%
robot_valid_directions({RobotPid, MarsPid}) ->
  %% First set of directions
  Directions1 = "LFRFF",
  {XPos1, YPos1, Orientation1} = input_robot_directions({RobotPid, MarsPid}, Directions1),

  %% Second set of directions
  ResetResult = robot:set_location(RobotPid, {2, 3, "N"}),
  Directions2 = "FLLFR",
  {XPos2, YPos2, Orientation2} = input_robot_directions({RobotPid, MarsPid}, Directions2),

  [?_assertMatch(XPos1, 4), ?_assertMatch(YPos1, 4), ?_assertMatch(Orientation1, "E"),
    ?_assertMatch(ResetResult, {ok, {2, 3, "N"}}), ?_assertMatch(XPos2, 2), 
    ?_assertMatch(YPos2, 3), ?_assertMatch(Orientation2, "W")].

robot_invalid_directions({RobotPid, MarsPid}) ->
  % First set of directions
  ResetResult1 = robot:set_location(RobotPid, {0, 2, "N"}),
  Directions1 = "FFLFRFF",
  Result1 = input_robot_directions({RobotPid, MarsPid}, Directions1),

  %% Second set of directions
  ResetResult2 = robot:set_location(RobotPid, {1, 0, "S"}),
  Directions2 = "FFRLF",
  Result2 = input_robot_directions({RobotPid, MarsPid}, Directions2),

  [?_assertMatch(ResetResult1, {ok, {0, 2, "N"}}), ?_assertMatch(Result1, {lost, {0, 4, "W"}}), 
    ?_assertMatch(ResetResult2, {ok, {1, 0, "S"}}), ?_assertMatch(Result2, {lost,{1, 0, "S"}})].
%%% Directions %%%

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helper Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%
input_robot_directions({RobotPid, MarsPid}, [Head | Tail]) ->
  case robot:change_location(RobotPid, Head, MarsPid) of
    {ok, {safe, {_, _, _}}} ->
      input_robot_directions({RobotPid, MarsPid}, Tail);
    {error, {lost, {XPos, YPos, Orientation}}} ->
      {lost, {XPos, YPos, Orientation}}
  end;

input_robot_directions({RobotPid, _}, []) ->
  {ok, Result} = robot:get_location(RobotPid),
  Result.