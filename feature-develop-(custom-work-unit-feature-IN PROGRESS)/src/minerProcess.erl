%%%-------------------------------------------------------------------
%%% @author lohit
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2022 10:36 am
%%%-------------------------------------------------------------------

-module(minerProcess).
-author("lohit").

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
%%  start_link().

%%% ==== Internal APIs not to be exposed
main_loop() ->
  receive
    {ok,MinerPid,InputString,Integer, RemCoins} ->
      io:format("In Miner Area ~n"),
      io:format("MinerPid is ~p ~n",[MinerPid]),
      io:format("Minned ~p hashed result is ~p ~n",[InputString, Integer]),
      io:format("Remaining coins to find ~p ~n",[RemCoins]),
      io:format("~n")
%%    {status, RemCoins} ->
%%      io:format("Remaining coins to find ~p ~n",[RemCoins])
  end.