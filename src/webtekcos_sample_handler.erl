-module(webtekcos_sample_handler).
-export([connect/0, disconnect/1, handle_data/2, handle_message/2]).

connect() -> 
  erlang:start_timer(1000, self(), ?MODULE),
  {counter, 1}.

disconnect({counter, _}) ->
  {counter, 0}.

handle_data(Data, {counter, Counter}) ->
  webtekcos:send_data(list_to_binary(Data)),

  {counter, case Counter of
    10 -> 
      webtekcos:send_data(list_to_binary("Good Bye !!!")),
      webtekcos:close(),
      0;
    _ -> 
      Counter + 1
  end}.

handle_message({timeout, _, ?MODULE}, LoopData) ->
  webtekcos:send_data(list_to_binary("Hello Timer")),
  io:format("<<<<< ~p~n", ["Hello Timer"]),
  LoopData.
