-module(server).
-author("User").

-export( [ main_loop/3, start_process/2] ).

main_loop(Count, LeadingZeros, Curr_Count) ->
  receive
    {ok,_,InputString,Integer, _, _, Coin_Origin} ->

%%      Analysing the origin of bitcoin (whether master / worker)
      PidStr = pid_to_list(Coin_Origin),
      PidStr1 = lists:sublist(PidStr, 2, length(PidStr)-2),
      [N, _, _] = [list_to_integer(T) || T <- string:tokens(PidStr1,[$.])],

      if
%%        Printing coin
        Curr_Count =< Count ->
          io:format("~n"),
          io:format("---------"),
          io:format("Coin #~p Mined! ~n",[Curr_Count]),

%%          The miner PID can be used to see if coin came from master or worker. The worker's and master's PIDs are printed in their respective terminal.
          io:format("Mined by Process PID : ~p ~n",[Coin_Origin]),
          io:format("Mined ~p hashed result is ~p ~n",[InputString, Integer]),
          io:format("~n"),
          io:format("~n"),

          if
          %%      If Node number in PID is >= 1, then it is a worker process (external node)
          %%   If Node number in PID is =0 , then it is the master itself
            N >= 1 ->
%%              send message to keep mining
              Coin_Origin ! {ok, "mine_more"};

            N == 0 ->
              %%              keep mining at master side
              Work_unit = 8,
              cryptoManager:startMining(self(),Work_unit,LeadingZeros,self());
            true -> nothing
          end,

          if
            Curr_Count == Count ->
              {_,FinalRealTime} = statistics(wall_clock),
              {_,FinalCPUTime} =statistics(runtime),
              io:format("Final Real Time is ~p milliseconds ~n",[FinalRealTime]),
              io:format("Final CPU time is ~p milliseconds ~n",[FinalCPUTime]),
              io:format("The final ratio is ~p ~n",[FinalCPUTime/FinalRealTime]);
            true -> nothing
%%            true -> sample
          end,


          main_loop(Count, LeadingZeros, Curr_Count+1);

        true -> nothing

      end
  end.


start_process(Count,LeadingZeros) ->
  Work_unit = 8,

%%  allow workers to connect at start/ midway of mining
  Server_PID = spawn_link(?MODULE,main_loop,[Count, LeadingZeros, 1]),
  io:format("The local process ID for workers to connect : ~p. ~n",[Server_PID]),
  io:format("The PID of Master is : ~p. ~n",[self()]),
  io:format("Please note the above. Waiting for 25 seconds before starting mining. Workers may connect mid-way while mining. ~n"),
  timer:sleep(25000),

%%  #starting mining at master
  statistics(wall_clock),
  statistics(runtime),

%%  cryptoManager manages 'Work_unit' number of Cryptoworkers that would mine coins
  cryptoManager:startMining(Server_PID,Work_unit,LeadingZeros, self()).









