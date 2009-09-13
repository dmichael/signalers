%%%-------------------------------------------------------------------
%%% File    : actuator.erl
%%% Author  : David Michael <david.michael@gmail.com>
%%% Description : Actuator for Signalers
%%%
%%% Created :  
%%%-------------------------------------------------------------------
-module(actuator).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1, actuate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ParentPid) ->
  gen_server:start_link(?MODULE, [ParentPid], []).
  
stop(Pid) ->
    gen_server:cast(Pid, stop).
    
actuate(Pid, Data) ->
  io:format("~p (~p) received ~p actuate from ~p.~n", [?MODULE, self(), Data, Pid]),
  
  % io:format("~p (~p) received actuate from (~p) ...~n", [?MODULE, self(), Pid]),
  gen_server:cast(Pid, Data).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([ParentPid]) ->
  io:format("~p (~p) starting ...~n", [?MODULE, self()]),
  erlang:monitor(process, ParentPid),
  State = dict:store(actuator_fsm, ParentPid, dict:new()),
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
    
handle_cast({signal, Period}, State) ->
  io:format("~p (~p) signaling ...~n", [?MODULE, self()]),
  
  % this should be managed differently
  SignalLength = Period - 2500,
  timer:sleep(SignalLength),
  io:format("~p (~p) done.~n", [?MODULE, self()]),
  
  {_Status, Pid} = dict:find(actuator_fsm, State),
  actuator_fsm:finished(Pid, []),
  {noreply, State};
  
handle_cast(stop, State) ->
    {stop, normal, State};
    
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
  io:format("parent down (actuator_fsm)!~n"),
  % stop myself if my actuator goes down
  {stop, normal, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

