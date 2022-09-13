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
-export([hashInput/2, matchString/2, main_loop/0]).

%%%% ==== Expose the API to client
start_link() ->
  %%% spawn_link the client with worker
  WorkerPid = spawn_link(?MODULE,main_loop,[]),
  io:format("Worker Started at Pid ~p ~n",[WorkerPid]),
  register(?MODULE,WorkerPid),
  {ok,WorkerPid}.

%%% ==== Internal APIs not to be exposed
main_loop() ->
  receive
    {mine,CollectorId,LeadingZeros} ->
      hashInput(CollectorId,LeadingZeros);

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

hashInput(CollectorId,LeadingZeros) ->

  %%% Generate a random string
  RandomStr = base64:encode(crypto:strong_rand_bytes(8)),

  %%% Create an input string with prefix
  InputString = "lohitBhambri;" ++ binary:bin_to_list(RandomStr),

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
      io:format("Input String's ~p hashed result is ~p ~n",[InputString, Integer]),
      CollectorId ! {ok,InputString,Integer},
      %%% Start the main loop again
      main_loop();
    _ ->
      hashInput(CollectorId,LeadingZeros)
  end.
