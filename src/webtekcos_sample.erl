-module(webtekcos_sample).
-export([connect/0, disconnect/1, handle_data/2, handle_message/2]).

connect() -> 
  io:format("===== OPEN CONNECTION~n"),
  erlang:start_timer(1000, self(), ?MODULE),
  {counter, 1}.

disconnect({counter, _}) ->
  io:format("===== CLOSE CONNECTION~n"),
  {counter, 0}.

handle_data(Data, {counter, Counter}) ->
  webtekcos:send_data(list_to_binary(Data)),
  io:format("<<<<< ~p~n", [Data]),

  {counter, case Counter of
    10 -> 
      webtekcos:send_data(list_to_binary("Good Bye !!!")),
      io:format("<<<<< ~p~n", ["Good Bye !!!"]),
      webtekcos:close(),
      0;
    _ -> 
      Counter + 1
  end}.

handle_message({timeout, _, ?MODULE}, LoopData) ->
  webtekcos:send_data(list_to_binary("Hello Timer")),
  io:format("<<<<< ~p~n", ["Hello Timer"]),
  LoopData.
