-module(xtimer).
-export([make/2]).

make(Period, Message) -> spawn(fun () -> start(Period, Message) end).

start(Period, Message) ->
  receive
    NewMessage when is_list(NewMessage)  -> start(Period, NewMessage);
    NewPeriod  when is_number(NewPeriod) -> start(NewPeriod, Message);
    {apples, Count} ->
      io:format("Got ~p apples", [Count])
  after Period ->
    io:format("~p~n", [Message]),
    start(Period, Message)
  end.