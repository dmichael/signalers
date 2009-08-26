-module(actuator).
-behaviour(gen_fsm).

-export([start/1]).
-export([signal/1]).
-export([init/1, waiting/2, signaling/2, refracting/2]).

% start_link/4
%
% Start the FSM and register it locally as "actuator"
%
% The first argument {local, code_lock} specifies the name
% The second argument is the name of the callback module
% The third argument, Code, is a term which is passed as-is to the callback function init
% The fourth argument, [], is a list of options

start(Code) ->
  gen_fsm:start_link({local, actuator}, actuator, Code, []).

% ------
% Events
% ------

% gen_fsm:send_event(FsmRef, Event) -> ok
  
signal(Number) ->
  gen_fsm:send_event(actuator, Number).
      
% receive(StateData) ->
%   gen_fsm:send_event(actuator, {signal, StateData}).  

% The new gen_fsm process calls the callback function init. This function is expected to return {ok, StateName, StateData}, where StateName is the name of the initial state of the gen_fsm

init(Code) ->
  io:format("initializing actuator.~n"),
  {ok, waiting, {[], Code}}.

% -----------------
% State transitions = [ready, signaling, refracting]
% -----------------

% The state transition rules are written as a number of Erlang functions which conform to the following convention:
%
% StateName(Event, StateData) ->
%     .. code for actions here ...
%     {next_state, StateName', StateData'}
%
% There should be one instance of this function for each possible state name

waiting(Event, {current, State}) ->
  io:format("waiting.~n"),
  
  case {current, State} of
    {current, waiting} ->
      io:format("waiting.~n"),
      {next_state, signaling, {[], Code}}.
  end.  
  
  
  
  
signaling(Event, {[], Code}) ->
  io:format("signaling.~n"),
  % ... trigger the actuator ...
  {next_state, refracting, {[], Code}}.
  
  
refracting(Event, {[], Code}) ->
  io:format("refracting.~n"),
  {next_state, ready, {[], Code}}.
  