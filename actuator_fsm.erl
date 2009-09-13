%%%-------------------------------------------------------------------
%%% File    : actuator_fsm.erl
%%% Author  : David Michael <david.michael@gmail.com>
%%% Description : Actuator FSM for Signalers
%%%
%%% Created :  
%%%-------------------------------------------------------------------
-module(actuator_fsm).

-behaviour(gen_fsm).

%% API
-export([
  start_link/0,
  stimulate/1,
  trigger/2,
  finished/2,
  stop/1
]).

%% gen_fsm callbacks
-export([ready/2, signaling/2, refracting/2, ready/3]).

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: trigger(Pid, Data) -> ok
%% Description: Generic trigger input to the actuator
%%--------------------------------------------------------------------
trigger(Pid, Data) ->
  gen_fsm:send_event(Pid, {trigger, Data}).

finished(Pid, Data) ->
  gen_fsm:send_event(Pid, {finished, Data}).  
  
stimulate(Pid) ->
  io:format("heard something.~n"),
  gen_fsm:send_event(Pid, stimulate).
  
%%--------------------------------------------------------------------
%% Function: cancel/0
%% Description: Cancels the ATM transaction no matter what state.
%%--------------------------------------------------------------------
stop(Pid) ->
  gen_fsm:send_all_state_event(Pid, stop).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  io:format("~p (~p) starting ...~n", [?MODULE, self()]),
  
  Period = 3000,
  
  {_, ActuatorPid}   = actuator:start_link(self()),
  io:format("~p (~p) registered actuator (~p)...~n", [?MODULE, self(), ActuatorPid]),
  
  OscillatorPid = oscillator:spawn_link(self(), Period, {}),
  io:format("~p (~p) registered oscillator (~p)...~n", [?MODULE, self(), OscillatorPid]),
    
  % erlang:monitor(process, ActuatorPid),
  % The actuator can still be orphaned if this FSM goes down... how do we avoid this?
  
  StateData  = dict:store(period, Period, dict:new()),
  StateData1 = dict:store(actuator, ActuatorPid, StateData),
  StateData2 = dict:store(oscillator, OscillatorPid, StateData1),
  
  {ok, ready, StateData2}.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------

%%-------------------------
%% READY
%%-------------------------

ready({trigger, _Data}, StateData) ->
  io:format("~p (~p) event received: ready -> signaling~n", [?MODULE, self()]),
  
  ActuatorPid = dict:fetch(actuator, StateData),
  Period = dict:fetch(period, StateData),

  actuator:actuate(ActuatorPid, {signal, Period}),
  {next_state, signaling, StateData};
  
ready(timeout, StateData) ->
  % reset the signaler for next signal
  io:format("ready(timeout)~n"),
  {next_state, ready, StateData};
  
ready(stimulate, StateData) ->
  io:format("ready(stimulate)~n"),
  {_, Pid} = dict:find(oscillator, StateData),
  Pid ! stimulate,
  {next_state, ready, StateData};
  
ready(_Event, StateData) ->
  {next_state, ready, StateData}.

%%-------------------------
%% SIGNALING
%%-------------------------

% In the signaling state we match on a finished message to change state
signaling({finished, _Data}, StateData) ->
  io:format("~p (~p) event received (finished): signaling -> ready~n", [?MODULE, self()]),
  %{_Status, Timeout} = dict:find(period, StateData),
  {next_state, ready, StateData};
  
% In the signaling state if a trigger is received, there is nothing to do.
signaling(_Event, StateData) ->
  {next_state, signaling, StateData}.

%%-------------------------
%% REFRACTING
%%-------------------------

% refracting({trigger, Data}, StateData) ->
%   {_Status, Timeout} = dict:find(timeout, StateData),
%   io:format("refracting, nothing to do, will timeout automatically~p~n", [Timeout]),
%   {next_state, ready, StateData, Timeout};
  
% catch all
refracting(_Event, StateData) ->
  {_Status, Timeout} = dict:find(period, StateData),
  io:format("refracting, nothing to do, will timeout automatically~p~n", [Timeout]),
  {next_state, refracting, StateData, Timeout}.  
  

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
ready({trigger, _Data}, _From, State) ->
  io:format("ready -> signaling"),
  {reply, ok, signaling, State}.


%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, NextState} |
%%                                          {next_state, NextStateName, NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(stop, _StateName, StateData) ->
  {stop, normal, StateData};
handle_event(_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, _StateName, State) ->
  io:format("man down (actuator)!~n"),
  % stop myself if my actuator goes down
  {stop, normal, State};
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------