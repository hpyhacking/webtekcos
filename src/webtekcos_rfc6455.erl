-module(webtekcos_rfc6455).
-export([handshake/2, encode/1, decode/1]).

-define(WEBSOCKET_PREFIX,
  "HTTP/1.1 101 Switching Protocols\r\n" ++ 
  "Upgrade: websocket\r\nConnection: Upgrade\r\n").

handshake(Socket, Headers) ->
  Key = webtekcos_tools:get_header(Headers, "Sec-Websocket-Key"),
  Protocol = webtekcos_tools:get_header(Headers, "Sec-Websocket-Protocol"),
  Accept = generate_websocket_accept(Key),

  Resp = ?WEBSOCKET_PREFIX ++
  "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n" ++
  "Sec-WebSocket-Protocol: " ++ Protocol ++ "\r\n\r\n" ,

  gen_tcp:send(Socket, Resp).

encode(Data) when is_binary(Data) ->
  coding_data(Data, size(Data)).

decode(Data) when is_binary(Data) ->
  decoding_data(Data).

decoding_data(<<_Fin:1, _Rsv:3, 8:4, _/binary>>) -> tcp_closed;
decoding_data(<<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, 126:7, _Size:16, MaskKey:32, Msg/binary>>) ->
  unmask_data(binary_to_list(Msg), <<MaskKey:32>>, 4, []);
decoding_data(<<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, _Size:7, MaskKey:32, Msg/binary>>) ->
  unmask_data(binary_to_list(Msg), <<MaskKey:32>>, 4, []).

coding_data(Bin, Size) when Size =< 125 ->
  %%       FIN  RSV  OPCODE  MASK  SIZE    DATA
  <<1:1, 0:3, 1:4,    0:1,  Size:7, Bin/binary>>;
coding_data(Bin, Size) ->
  %%       FIN  RSV  OPCODE  MASK  SIZE            DATA
  <<1:1, 0:3, 1:4,    0:1,  126:7, Size:16, Bin/binary>>.

unmask_data([], _MaskKey, _Index, Result) ->
  lists:reverse(Result);
unmask_data([H|T], MaskKey, Index, Result) ->
  Unmask = H bxor binary:at(MaskKey, Index rem 4),
  unmask_data(T, MaskKey, Index + 1, [Unmask|Result]).

generate_websocket_accept(Key) ->
  base64:encode_to_string(crypto:sha(
    Key ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")).

  %Origin = get_header(Headers, "Sec-Websocket-Origin")
  %Location = get_header(Headers, 'Host')
