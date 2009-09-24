-module(fac).
-export([call/1]).

call(X) ->
  {any, 'c1@computerone'} ! {self(), X},
  receive
    {ok, Result} ->
      Result
  end.