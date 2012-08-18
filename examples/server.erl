#!/usr/bin/env escript

-export([main/1]).

main(_) ->
  code:add_patha("ebin"),
  code:add_patha("deps/mochiweb/ebin"),

  application:start(sasl),
  application:start(webtekcos),

  webtekcos_event_logger:add_handler(),

  io:format("Open client.html test websocket server.~n"),
  io:format("It's listen on localhost:3008.~n"),
  io:format("Press Ctrl+C to shutdown server!!!~n"),

  os:cmd("open ./examples"),

  receive
    _ -> ok
  end.
