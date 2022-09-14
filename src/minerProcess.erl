%%%-------------------------------------------------------------------
%%% @author lohit
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Sep 2022 12:28 am
%%%-------------------------------------------------------------------
-module(minerProcess).
-author("lohit").

%% API
-export([]).

%% API
-export([start_link/0]).
-export([main_loop/0]).

%%% register(?MODULE,SupervisorPid)

%%%% ==== Expose the API to client
start_link() ->
  %%% spawn_link the client with worker
  MinerPid = spawn_link(?MODULE,main_loop,[]),
  %%% io:format("Miner Pid is ~p ~n",[MinerPid]),
  {ok,MinerPid}.

%%% ==== Internal APIs not to be exposed
main_loop() ->
  receive
    {ok,MinerPid,InputString,Integer} ->
      io:format("In Miner Area ~n"),
      io:format("MinerPid is ~p ~n",[MinerPid]),
      io:format("Minned ~p hashed result is ~p ~n",[InputString, Integer]),
      io:format("~n")
  end.