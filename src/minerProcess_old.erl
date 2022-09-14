%%%-------------------------------------------------------------------
%%% @author lohit
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2022 09:47 pm
%%%-------------------------------------------------------------------
-module(minerProcess_old).
-author("lohit").

%% API
-export([receiveWorkerPid/0]).

%%% register(?MODULE,SupervisorPid)

receiveWorkerPid() ->
  receive
    {worker,WorkerPid} ->
      WorkerPid;

    terminate ->
      io:format("Nothing")
  end.