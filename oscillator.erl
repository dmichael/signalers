-module(oscillator).
-export([spawn_link/3]).

spawn_link(ParentPid, Period, Message) -> 
  % I Want to Die If a Process I Create Crashes
  spawn_link(fun () -> start(ParentPid, Period, Message) end).

cancel(Pid) -> Pid ! cancel.

start(ParentPid, Period, Message) ->
  receive
    NewMessage when is_list(NewMessage)  -> start(ParentPid, Period, NewMessage);
    NewPeriod  when is_number(NewPeriod) -> start(ParentPid, NewPeriod, Message);
    stimulate ->
      io:format("~p (~p) stimulus received~n", [?MODULE, self()]),
      % inhibit/reset, that is, just start it again
      start(ParentPid, Period, Message)
  after Period ->
    io:format("~p (~p) ping. ~n", [?MODULE, self()]),
    actuator_fsm:trigger(ParentPid, {}),
    start(ParentPid, Period, Message)
  end.