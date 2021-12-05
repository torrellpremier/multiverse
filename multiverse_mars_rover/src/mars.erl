-module(mars).
-behaviour(gen_server).

%% mars interface
-export([
  start/2,
  get_grid_dimensions/1,
  validate_rover_position/3,
  stop/1
]).

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
start(Width, Height) ->
  gen_server:start_link(?MODULE, {Width, Height}, []).

get_grid_dimensions(Pid) ->
  gen_server:call(Pid, get_grid_dimensions).

validate_rover_position(Pid, XPos, YPos) ->
  gen_server:call(Pid, {validate_rover_position, XPos, YPos}).

stop(Pid) ->
  gen_server:stop(Pid).
%------------------------Public interface-------------------------%

%----------------------gen_server callbacks-----------------------%
init({Width, Height}) ->
  StateData = #{width => Width, height => Height},
  {ok, StateData}.

handle_call(get_grid_dimensions, _From, StateData = #{width := Width, height := Height}) ->
  {reply, {ok, {Width, Height}}, StateData};

handle_call({validate_rover_position, XPos, YPos}, _From, 
StateData = #{width := Width, height := Height}) ->
  case validate_position(Width, XPos) of
    valid ->
      case validate_position(Height, YPos) of
        valid ->
          {reply, {ok, safe}, StateData};
        invalid ->
          {reply, {ok, lost}, StateData}
      end;
    invalid ->
      {reply, {ok, lost}, StateData}
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
validate_position(Position, ValidatedPosition) ->
  if
    ValidatedPosition > Position -> invalid;
    ValidatedPosition < 0 -> invalid;
    true -> valid
  end.
%-----------------------Private functions------------------------%
