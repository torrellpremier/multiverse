-module(robot_manager).
-behaviour(gen_statem).

% robot_manager interface
-export([
  start/0,
  stop/1
]).

% gen_statem callbacks
-export([
  callback_mode/0,
  code_change/4,
  init/1,
  terminate/3
]).

% gen_statem states
-export([
  setup_mars_and_rovers/3,
  calculating_positions/3
]).

%------------------------Public interface-------------------------%
start() ->
  %% Grab the Mars Dimensions
  {ok, [MarsWidth, MarsHeight]} = io:fread("", "~d~d"),
  %% Grab the first rover's initial coordinates and commands
  {ok, [_, XPos1, YPos1, Orientation1, _, CommandList1]} = 
    io:fread("", "~c~d,~d, ~c~c ~s"),
  %% Grab the second rover's initial coordinates and commands
  {ok, [_, XPos2, YPos2, Orientation2, _, CommandList2]} = 
    io:fread("", "~c~d,~d, ~c~c ~s"),
  gen_statem:start_link(?MODULE, [{MarsWidth, MarsHeight},
    {XPos1, YPos1, Orientation1, CommandList1},
    {XPos2, YPos2, Orientation2, CommandList2}], []).

stop(Pid) ->
  gen_statem: stop(Pid).
%------------------------Public interface-------------------------%

%----------------------gen_statem callbacks-----------------------%
init(InitData) ->
  StateData = #{rover_1_pid => null, rover_2_pid => null, mars_pid => null,
    rover_1_command_list => [], rover_2_command_list => [], init_data => InitData},

  {ok, setup_mars_and_rovers, StateData}.

callback_mode() -> 
    [state_functions, state_enter].

code_change(_Vsn, State, StateData, _Extra) ->
    {ok, State, StateData}.

terminate(_Reason, _State, _StateData = #{rover_1_pid := Rover1Pid,
rover_2_pid := Rover2Pid, mars_pid := MarsPid}) ->
  ok = robot:stop(Rover1Pid),
  ok = robot:stop(Rover2Pid),
  ok = mars:stop(MarsPid),
  normal.
%----------------------gen_statem callbacks-----------------------%

%-----------------------State functions------------------------%
setup_mars_and_rovers(enter, _, StateData = #{init_data := InitData}) ->
  [{MarsWidth, MarsHeight},
    {XPos1, YPos1, Orientation1, CommandList1},
    {XPos2, YPos2, Orientation2, CommandList2}]= InitData,

  {ok, MarsPid} = mars:start(MarsWidth, MarsHeight),
  {ok, Rover1Pid} = robot:start(XPos1, YPos1, Orientation1),
  {ok, Rover2Pid} = robot:start(XPos2, YPos2, Orientation2),

  {keep_state, StateData#{rover_1_pid := Rover1Pid, rover_2_pid := Rover2Pid,
    mars_pid := MarsPid, rover_1_command_list := CommandList1, 
    rover_2_command_list := CommandList2}, [{state_timeout, 1, calculating_positions}]};

setup_mars_and_rovers(state_timeout, calculating_positions, StateData) ->
  {next_state, calculating_positions, StateData}.

calculating_positions(enter, _, StateData = #{rover_1_pid := Rover1Pid, rover_2_pid :=
Rover2Pid, mars_pid := MarsPid, rover_1_command_list := CommandList1, 
rover_2_command_list := CommandList2}) ->
  Rover1Result = input_rover_directions({Rover1Pid, MarsPid}, CommandList1),
  Rover2Result = input_rover_directions({Rover2Pid, MarsPid}, CommandList2),

  case Rover1Result of
    {safe, {XPos1, YPos1, Orientation1}} ->
      io:format("(~p, ~p, ~p)~n", [XPos1, YPos1, Orientation1]);
    {lost, {XPos1, YPos1, Orientation1}} ->
      io:format("(~p, ~p, ~p) LOST~n", [XPos1, YPos1, Orientation1])
  end,
  case Rover2Result of
    {safe, {XPos2, YPos2, Orientation2}} ->
      io:format("(~p, ~p, ~p)~n", [XPos2, YPos2, Orientation2]);
    {lost, {XPos2, YPos2, Orientation2}} ->
      io:format("(~p, ~p, ~p) LOST~n", [XPos2, YPos2, Orientation2])
  end,
{keep_state, StateData};

calculating_positions(EventType, EventContent, StateData) ->
    handle_common(calculating_positions, EventType, EventContent, StateData).
%-----------------------State functions------------------------%

%-----------------------Private functions------------------------%
input_rover_directions({RobotPid, MarsPid}, [Head | Tail]) ->
  case robot:change_location(RobotPid, Head, MarsPid) of
    {ok, {safe, {_, _, _}}} ->
      input_rover_directions({RobotPid, MarsPid}, Tail);
    {error, {lost, {XPos, YPos, Orientation}}} ->
      {lost, {XPos, YPos, Orientation}}
  end;

input_rover_directions({RobotPid, _}, []) ->
  {ok, Result} = robot:get_location(RobotPid),
  {safe, Result}.

handle_common(State, EventType, EventContent, _) ->
    io:format("robot_manager_state state: '~p' - unhandled event: '~p', '~p'~n",
        [State, EventType, EventContent]),
    keep_state_and_data.
%-----------------------Private functions------------------------%
