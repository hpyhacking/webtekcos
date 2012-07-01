#!/usr/bin/env escript

-export([main/1]).

main(_) ->
  code:add_patha("ebin"),
  code:add_patha("deps/mochiweb/ebin"),

  webtekcos:start_link("127.0.0.1", 8000, webtekcos_sample),

  io:format("Open client.html test websocket server.~n"),
  io:format("It's listen on localhost:8000.~n"),
  io:format("Press Ctrl+C to shutdown server!!!~n"),

  os:cmd("open ./examples"),

  receive
    _ -> ok
  end.
