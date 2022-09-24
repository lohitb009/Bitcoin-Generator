-module(minerProcess).
-author("lohit").

%% API
-export([start_link/0]).
-export([main_loop/0]).


%%%% ==== Expose the API to client
start_link() ->
  %%% spawn_link the client with worker
  MinerPid = spawn_link(?MODULE,main_loop,[]),
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