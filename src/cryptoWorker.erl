-module(cryptoWorker).
-author("lohit").

-export([start_link/0]).
-export([hashInput/3, matchString/2, main_loop/0]).

%%%% ==== Expose the API to client
start_link() ->
  %%% spawn the cryptoworker
  WorkerPid = spawn_link(?MODULE,main_loop,[]),
  {ok,WorkerPid}.

%%% ==== Internal APIs not to be exposed
main_loop() ->
%%  forward request to hashInput()
  receive
    {mine,CollectorId,LeadingZeros,Requestor} ->
      hashInput(CollectorId,LeadingZeros,Requestor);

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

hashInput(CollectorId,LeadingZeros, Requestor) ->

  %%% Generate a random string
  RandomStr = base64:encode(crypto:strong_rand_bytes(8)),

  %%% Create an input string with prefix
  InputString = "lohit.bhambri;" ++ binary:bin_to_list(RandomStr),

  %%% Hash the input string
  <<Int:256>> = crypto:hash(sha256, InputString),
  Integer = io_lib:format("~64.16.0b", [Int]),

  Prefix = string:left(Integer,LeadingZeros),

  case matchString(Prefix,LeadingZeros) of
%%    check if matching number of zeroes
    true ->
%%      sending results to server
      CollectorId ! {ok,CollectorId,InputString,Integer, 1, 1, Requestor};
    _ ->
      hashInput(CollectorId,LeadingZeros, Requestor)
  end.
