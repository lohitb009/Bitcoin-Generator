-module(cryptoManager).
-author("lohit").

-export([startMining/4]).

startMining(HeadProcessId,Count,LeadingZeros,Requestor) ->
  forLoop(HeadProcessId,Count,LeadingZeros,Requestor).


forLoop(_,0,_,_) ->
%%  io:format("Finished spawning processes ~n");
  nothing;
forLoop(HeadProcessId,Count,LeadingZeros,Requestor) ->

  %%% Spawn the worker
  {ok,WorkerPid} = cryptoWorker:start_link(),

  %%% Send the message
  WorkerPid ! {mine,HeadProcessId,LeadingZeros,Requestor},

  forLoop(HeadProcessId,Count-1,LeadingZeros,Requestor).