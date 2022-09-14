%%%-------------------------------------------------------------------
%%% @author lohit
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Sep 2022 12:27 am
%%%-------------------------------------------------------------------
-module(forLoop).
-author("lohit").

%% API
-export([forLoop/2]).

forLoop(0,_) ->
  io:format("Done");

forLoop(Count,LeadingZeros) ->
  %%% Spawn the miners
  {ok,MinerPid} = minerProcess_old:start_link(),

  %%% Spawn the worker
  {ok,WorkerPid} = cryptoWorker:start_link(),

  %%% Send the message
  WorkerPid ! {mine,MinerPid,LeadingZeros},

  %%% Sleep for 1 secs
  %%% timer:sleep(1000),

  forLoop(Count-1,LeadingZeros).