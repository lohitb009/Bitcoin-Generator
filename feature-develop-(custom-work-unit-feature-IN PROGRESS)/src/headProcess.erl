%%%-------------------------------------------------------------------
%%% @author lohit
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Sep 2022 01:54 am
%%%-------------------------------------------------------------------
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
    60000 -> io:format("Timeout")
  end.