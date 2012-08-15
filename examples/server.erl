#!/usr/bin/env escript

-export([main/1]).

main(_) ->
  code:add_patha("ebin"),
  code:add_patha("deps/mochiweb/ebin"),

  webtekcos_app:start(),

  io:format("Open client.html test websocket server.~n"),
  io:format("It's listen on localhost:3008.~n"),
  io:format("Press Ctrl+C to shutdown server!!!~n"),

  os:cmd("open ./examples"),

  %webtekcos:start_link("127.0.0.1", 8000),
  %webtekcos_event_logger:add_handler(),

  webtekcos_event_logger:add_handler(),

  appmon:start(),

  receive
    _ -> ok
  end.
