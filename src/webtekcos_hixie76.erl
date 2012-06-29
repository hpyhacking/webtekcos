-module(webtekcos_hixie76).
-export([handshake/2, encode/1, decode/1]).

-define(WEBSOCKET_PREFIX,
  "HTTP/1.1 101 WebSocket Protocol Handshake\r\n" ++ 
  "Upgrade: WebSocket\r\nConnection: Upgrade\r\n").

handshake(Socket, Headers) ->
  {K1, K2, K3} = {
    parse_key(webtekcos_tools:get_header(Headers, "Sec-Websocket-Key1")),
    parse_key(webtekcos_tools:get_header(Headers, "Sec-Websocket-Key2")),
    webtekcos_tools:get_header(Headers, "_EOH_")
  },


  CheckSum = erlang:md5(<<K1:32/big, K2:32/big, (list_to_binary(K3))/binary>>),

  Path = webtekcos_tools:get_header(Headers, "_PATH_"),
  Host = webtekcos_tools:get_header(Headers, "Host"),
  Origin = webtekcos_tools:get_header(Headers, "Origin"),
  Protocol = webtekcos_tools:get_header(Headers, "Sec-WebSocket-Protocol"),

  log([Path, Host, Origin, Protocol]),

  Response = ?WEBSOCKET_PREFIX ++
  "Sec-WebSocket-Location: " ++ "ws://" ++ Host ++ Path ++ "\r\n" ++
  "Sec-WebSocket-Origin: " ++ Origin ++ "\r\n" ++
  "Sec-WebSocket-Protocol: " ++ Protocol ++ "\r\n\r\n" ++ binary_to_list(CheckSum),

  io:format("Response ~p~n", [Response]),

  gen_tcp:send(Socket, Response).

parse_key(Key) ->
  parse_key(Key, [], 0).
parse_key([], Numbers, 0) ->
  erlang:list_to_integer(lists:reverse(Numbers));
parse_key([], Numbers, Spaces) ->
  erlang:list_to_integer(lists:reverse(Numbers)) div Spaces;
parse_key([32 | Key], Numbers, Spaces) ->
  parse_key(Key, Numbers, Spaces + 1);
parse_key([C | Key], Numbers, Spaces) when C > 47 andalso C < 58 ->
  parse_key(Key, [C|Numbers], Spaces);
parse_key([_ | Key], Numbers, Spaces) ->
  parse_key(Key, Numbers, Spaces).

encode(Data) ->
  <<0, Data/binary, 255>>.

decode(<<0:8, End/binary>>) ->
  [Data, _] = binary:split(End, <<255>>),
  binary_to_list(Data).

log(Msg) ->
  io:format("===> ~p~n", [Msg]).
