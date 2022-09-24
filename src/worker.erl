-module(worker).
-author("User").

-export([main_loop/3]).

main_loop(join, LeadingZeroes, ServerPid) ->

  Work_unit = 8,
%%  %%  cryptoManager manages 'Work_unit' number of Cryptoworkers that would mine coins,
  cryptoManager:startMining(ServerPid,Work_unit,LeadingZeroes, self()),
  receive
    {ok, "mine_more"} ->
      main_loop(join, LeadingZeroes, ServerPid)
  end.



