%%%-------------------------------------------------------------------
%%% @author lohit
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Sep 2022 08:25 pm
%%%-------------------------------------------------------------------
-module(forLoop).
-author("lohit").

%% API
-export([forLoop/4]).

receiveMinerPid(HeadProcessId,ForLoopPid) ->
  HeadProcessId ! {ok,ForLoopPid,needMinerId},
  receive
    {ok,MinerPid} ->
      {ok,MinerPid}
  end.

forLoop(_,0,_,_) ->
  io:format("Done");
forLoop(HeadProcessId,Count,LeadingZeros,CoinsReqFromEach) ->

  %%% Spawn the miners

  %%%% 1. Send the message to headProcess ---- headPid ! {ok,self(),needMinerId} --- then wait for async call --- receive {ok,forLoopId,MinerPid}
  %%%% 2. headProcess will spawn a minerProcess and send the MinerPid to forLoop --- forLoopPid ! {ok,MinerPid}
  %%%$ 3. once the forLoopPid get the MinerPid and continueProcess
  %%%% 4. {ok,MinerPid} = receiveMinerPid(self())



  {ok,MinerPid} = receiveMinerPid(HeadProcessId,self()),

  %%%% {ok,MinerPid} = minerProcess:start_link(),

  %%% Spawn the worker
  {ok,WorkerPid} = cryptoWorker:start_link(),

  %%% Send the message
  WorkerPid ! {mine,MinerPid,LeadingZeros,CoinsReqFromEach},

  %%% Sleep for 1 secs
  %%% timer:sleep(1000),

  forLoop(HeadProcessId,Count-1,LeadingZeros,CoinsReqFromEach).