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
-export([startMining/3]).

receiveMinerPid(HeadProcessId,ForLoopPid) ->
  HeadProcessId ! {ok,ForLoopPid,needMinerId},
  receive
    {ok,MinerPid} ->
      {ok,MinerPid}
  end.

startMining(HeadProcessId,Count,LeadingZeros) ->
  {InitRealTime,_} = statistics(wall_clock),
  {InitCPUTime,_} =statistics(runtime),
  io:format("Initial Real Time is ~p milliseconds ~n",[InitRealTime]),
  io:format("Initial CPU time is ~p milliseconds ~n",[InitCPUTime]),

%%  io:format("Initial wall clock is ~p ~n",[statistics(wall_clock)]),
%%  io:format("Initial CPU time is ~p ~n",[statistics(runtime)]),
  forLoop(HeadProcessId,Count,LeadingZeros).


forLoop(_,0,_) ->
  io:format("Finished spawning processes ~n");
forLoop(HeadProcessId,Count,LeadingZeros) ->

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
  WorkerPid ! {mine,MinerPid,LeadingZeros,Count},


  %%% Sleep for 1 secs
  %%% timer:sleep(1000),

  forLoop(HeadProcessId,Count-1,LeadingZeros).