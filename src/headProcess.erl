-module(headProcess).
-author("lohit").

%% API
-export([receiveLoopRequest/0]).

receiveLoopRequest() ->
  receive

    {ok,ForLoopId,needMinerId} ->

      %%%% spawn the miner
      {ok,MinerPid} = minerProcess:start_link(),
      ForLoopId ! {ok,MinerPid},
      receiveLoopRequest();

    terminate ->
      io:format("Nothing")

  after
    10000 -> io:format("Timeout")
  end.