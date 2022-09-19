%%%-------------------------------------------------------------------
%%% @author lohit
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Sep 2022 10:36 am
%%%-------------------------------------------------------------------
-module(cryptoWorker).
-author("lohit").

%% API
-export([start_link/0]).
-export([hashInput/3, matchString/2, main_loop/0]).

%%%% ==== Expose the API to client
start_link() ->
  %%% spawn_link the client with worker
  WorkerPid = spawn_link(?MODULE,main_loop,[]),
  io:format("Worker Pid is ~p ~n",[WorkerPid]),
  {ok,WorkerPid}.

%%% ==== Internal APIs not to be exposed
main_loop() ->
  receive
    {mine,CollectorId,LeadingZeros,CoinsReq} ->
      %%% io:format("Collector Id ~p ~n",[CollectorId]),
      hashInput(CollectorId,LeadingZeros,CoinsReq);


    terminate ->
      io:format("Stop the server")
  end.

matchString([],_)->
  true;
matchString([Head|Tail],LeadingZeros) ->
  case Head of
    %%% Ascii Value of Zero
    48  -> matchString(Tail,LeadingZeros-1);
    _ -> false
  end.

hashInput(CollectorId,LeadingZeros,0) ->

  io:format("Done - found all coins~n");
%%  CollectorId ! {status, 0};
%%  CollectorId ! {ok,CollectorId,"I have finished"," ",0};

hashInput(CollectorId,LeadingZeros,CoinsReq) ->
%%  io:format("Coinsreq is: ~p ~n",[CoinsReq]),
%%  CollectorId ! {ok,CollectorId,"I have started"," ",CoinsReq},
%%  io:format("I have started, coins req are ~p ~n",[CoinsReq]), IMP
%%  io:format("~n"), IMP

  %%% Generate a random string
  RandomStr = base64:encode(crypto:strong_rand_bytes(8)),

  %%% Create an input string with prefix
  InputString = "lohit.bhambri;" ++ binary:bin_to_list(RandomStr),

  %%% Print the string
  %%% io:format("InputString is: ~p ~n",[InputString]),

  %%% Hash the input string
  <<Int:256>> = crypto:hash(sha256, InputString),
  Integer = io_lib:format("~64.16.0b", [Int]),
  %%% io:format("Hashed Integer is: ~p ~n",[Integer]),

  Prefix = string:left(Integer,LeadingZeros),
  %%% IMPORTANT : ONLY EXACTLY LEADING ZEROES
  %%% io:format("Prefix is: ~p ~n",[Prefix]),

%%  CollectorId ! {ok,CollectorId,"Before entering function"," ",CoinsReq},
%%  io:format("Before entering function, coins req are ~p ~n",[CoinsReq]), IMP
%%  io:format("~n"), IMP

  case matchString(Prefix,LeadingZeros) of
    true ->
      io:format("Input String's ~p hashed result is ~p ~n",[InputString, Integer]),
%%      CollectorId ! {ok,CollectorId,InputString,Integer,CoinsReq-1},
%%      CollectorId ! {status, CoinsReq-1},
      hashInput(CollectorId,LeadingZeros,CoinsReq-1);
%%      IMP START
%%      io:format("After finish of function, coins req are ~p ~n",[CoinsReq]),
%%      io:format("~n");
%%    IMP STOP
%%      CollectorId ! {ok,CollectorId,"After finish of function"," ",CoinsReq-1};
    false ->
      hashInput(CollectorId,LeadingZeros,CoinsReq)
  end.
