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
    {mine,CollectorId,LeadingZeros,Count} ->
      %%% io:format("Collector Id ~p ~n",[CollectorId]),
      hashInput(CollectorId,LeadingZeros,Count);


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

hashInput(CollectorId,LeadingZeros,Count) ->

%%  io:format("Wallclock time is ~p ~n",[erlang:trace(all, true, [timestamp])]),
%%  statistics(runtime),
%%  statistics(wall_clock),
%%  io:format("Initial wall clock is ~p ~n",[erlang:now()]),

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
  %%% io:format("Prefix is: ~p ~n",[Prefix]),

  case matchString(Prefix,LeadingZeros) of
    true ->
%%      {_, CPUTime} = statistics(runtime), IMP
%%      {_, RealTime} = statistics(wall_clock), IMP
      {CPUTime, _} = statistics(runtime),
      {RealTime, _} = statistics(wall_clock),
      %%% io:format("Input String's ~p hashed result is ~p ~n",[InputString, Integer]),
%%      io:format("Final wall clock is ~p ~n",[erlang:trace(all, true, [timestamp] )]),
%%      io:format("Final wall clock is ~p ~n",[erlang:now()]),
%%      if
%%        io:format("Coin# ~p CPU Time taken for execution: ~p milliseconds ~n",[CPUTime]), imp
%%          io:format("Coin# ~p Real Time taken for execution: ~p millisecondds~n",[RealTime]), imp
      io:format("Coin# ~p Current Total CPU Time: ~p milliseconds ~n",[Count, CPUTime]),
      io:format("Coin# ~p Current Total Real Time: ~p milliseconds ~n",[Count, RealTime]),
%%          io:format("Ratio of CPU Time to Real Time: ~p ~n",[CPUTime/RealTime]);
%%        true -> ok
%%      end,
      CollectorId ! {ok,CollectorId,InputString,Integer};
    _ ->
      hashInput(CollectorId,LeadingZeros,Count)
  end.
