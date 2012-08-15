-module(webtekcos_event_logger).

-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, code_change/3, terminate/2]).

add_handler() ->
  webtekcos_event:add_handler(?MODULE, []).

delete_handler() ->
  webtekcos_event:delete_handler(?MODULE, []).

init([]) ->
  {ok, []}.

handle_event({rcv_data, Data}, State) ->
  io:format("<<<<<< ~p~n", [Data]),
  {ok, State};

handle_event({send_data, Data}, State) ->
  io:format(">>>>>> ~p~n", [Data]),
  {ok, State};

handle_event({start, Args}, State) ->
  io:format("====== STARTING ~p ~n", [Args]),
  {ok, State};

handle_event(connect, State) ->
  io:format("<<<=== CONNECTION~n"),
  {ok, State};

handle_event(disconnect, State) ->
  io:format("===>>> DISCONNECTION~n"),
  {ok, State}.

handle_call(_Request, State) -> 
  {ok, ok, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Arg, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.
