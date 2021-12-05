-module(robot).
-behaviour(gen_server).

%% robot interface
-export([
  start/3,
  get_location/1,
  set_location/2,
  change_location/3,
  stop/1]).

%% gen_server callbacks
-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2, 
  terminate/2,
  code_change/3
]).

%------------------------Public interface-------------------------%
start(XPos, YPos, Orientation) ->
  gen_server:start_link(?MODULE, {XPos, YPos, Orientation}, []).

get_location(Pid) ->
  gen_server:call(Pid, get_location).

set_location(Pid, LocationInfo) ->
  gen_server:call(Pid, {set_location, LocationInfo}).

change_location(Pid, Command, MarsPid) ->
  gen_server:call(Pid, {change_location, Command, MarsPid}).

stop(Pid) ->
  gen_server:stop(Pid).
%------------------------Public interface-------------------------%

%----------------------gen_server callbacks-----------------------%
init({XPos, YPos, Orientation}) ->
  StateData = #{x_pos => XPos, y_pos => YPos, orientation => Orientation},
  {ok, StateData}.

handle_call(get_location, _From, StateData = 
#{x_pos := XPos, y_pos := YPos, orientation := Orientation}) ->
  {reply, {ok, {XPos, YPos, Orientation}}, StateData};

handle_call({set_location, {XPos, YPos, Orientation}}, _From, StateData) ->
  UpdatedStateData = StateData#{x_pos := XPos, y_pos := YPos, orientation := Orientation},
  {reply, {ok, {XPos, YPos, Orientation}}, UpdatedStateData};

handle_call({change_location, Command, MarsPid}, _From, StateData = 
#{x_pos := XPos, y_pos := YPos, orientation := Orientation}) ->
  %% Handle the command recieved
  case run_command(Command, XPos, YPos, Orientation) of
    {y_pos, UpdatedYPos} ->
      case mars:validate_rover_position(MarsPid, XPos, UpdatedYPos) of
        {ok, safe} ->
          {reply, {ok, {safe, {XPos, UpdatedYPos, Orientation}}}, 
            StateData#{y_pos := UpdatedYPos}};
        {ok, lost} ->
          {reply, {error, {lost, {XPos, YPos, Orientation}}}, StateData}
      end;
    {x_pos, UpdatedXPos} ->
      case mars:validate_rover_position(MarsPid, UpdatedXPos, YPos) of
        {ok, safe} ->
          {reply, {ok, {safe, {UpdatedXPos, YPos, Orientation}}},
            StateData#{x_pos := UpdatedXPos}};
        {ok, lost} ->
          {reply, {error, {lost, {XPos, YPos, Orientation}}}, StateData}
      end;
    {orientation, UpdatedOrientation} ->
      {reply, {ok, {safe, {XPos, YPos, UpdatedOrientation}}},
        StateData#{orientation := UpdatedOrientation}}
  end.

handle_info(_Info, StateData) ->
  {noreply, StateData}.

handle_cast(_Msg, StateData) ->
  {noreply, StateData}.

code_change(_OldVsn, _StateData, _Extra) ->
  {ok, new_state}.

terminate(_Reason, _StateData) ->
  ok.

%----------------------gen_server callbacks-----------------------%

%-----------------------Private functions------------------------%
run_command(Command, XPos, YPos, Orientation) ->
  case Command of
      70 -> % "F"
        case Orientation of
          "N" ->
            {y_pos, YPos + 1}; 
          "E" -> 
            {x_pos, XPos + 1};
          "S" ->
            {y_pos, YPos - 1};
          "W" -> 
            {x_pos, XPos - 1}
        end;
      76 -> % "L"
        case Orientation of
          "N" -> 
            {orientation, "W"};
          "E" ->
            {orientation, "N"};
          "S" -> 
            {orientation, "E"};
          "W" -> 
            {orientation, "S"}
        end;
      82 -> % "R"
        case Orientation of
          "N" -> 
            {orientation, "E"};
          "E" ->
            {orientation, "S"};
          "S" -> 
            {orientation, "W"};
          "W" -> 
            {orientation, "N"}
        end
    end.
%-----------------------Private functions------------------------%
