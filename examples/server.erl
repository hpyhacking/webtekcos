#!/usr/bin/env escript

-export([main/1]).

main(_) ->
  code:add_patha("ebin"),
  code:add_patha("deps/mochiweb/ebin"),
  Loop = fun(Msg, Counter) -> 
      case Msg of
        connected -> io:format("===== OPEN CONNECTION~n"), 1;
        disconnected -> io:format("====== CLOSE CONNECTION~n"), 0;
        {recv, Data} -> 
          case Data of 
            0 -> ok;
            _ ->
              self() ! {send, list_to_binary(Data)},
              io:format("<<<<< ~p~n", [Data]),

              case Counter of
                10 -> 
                  self() ! {send, list_to_binary("Good Bye !!!")},
                  self() ! close,
                  0;
                _ -> Counter + 1
              end
          end
      end
  end,
  webtekcos_websocket_server:start_link("127.0.0.1", 8000, Loop),
  io:format("Open client.html test websocket server.~n"),
  io:format("It's listen on localhost:8000.~n"),
  io:format("Press Ctrl+C to shutdown server!!!~n"),
  receive
    _ -> ok
  end.
