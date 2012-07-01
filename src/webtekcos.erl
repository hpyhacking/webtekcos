-module(webtekcos).
-export([start_link/0, start_link/3, start_link/4, stop/1, loop/2]).
-export([send_data/1, close/0]).

start_link() ->
  start_link(?MODULE, "127.0.0.1", 3008, webtekcos_sample).

start_link(Host, Port, Mod) when is_list(Host), is_integer(Port) ->
  start_link(?MODULE, Host, Port, Mod).
  
start_link(Name, Host, Port, Mod) when is_list(Host), is_integer(Port) ->
  Fun = fun (Socket) -> ?MODULE:loop(Socket, Mod) end,
  Options = [{ip, Host}, {loop, Fun}, {port, Port}, {name, Name}],
  io:format("start websocket server ...~n"),
  mochiweb_socket_server:start_link(Options).

stop(Name) ->
  mochiweb_socket_server:stop(Name).

%%%
%%% websocket protocol
%%%

handshake(Socket, Mod) ->
  inet:setopts(Socket, [{packet, http}]),
  case gen_tcp:recv(Socket, 0) of
    {ok, {http_request, _Method, {abs_path, Path}, _Version}} ->
      check_header(Socket, [{"_PATH_", Path}], Mod);
    _ ->
      log(invalid_handshake_request)
  end.

check_header(Socket, Headers, Mod) ->
  case gen_tcp:recv(Socket, 0, 50) of
    {ok, {http_header, _, Name, _, Value}} when is_atom(Name) ->
      log([fixed_header, Name, Value]),
      check_header(Socket, [{atom_to_list(Name), Value} | Headers], Mod);
    {ok, {http_header, _, Name, _, Value}} ->
      log([check_header, Name, Value]),
      check_header(Socket, [{Name, Value} | Headers], Mod);
    {ok, http_eoh} -> %% Check hixie76 last 8 byte checksum
      inet:setopts(Socket, [{packet, raw}]),
      check_header(Socket, Headers, Mod);
    {ok, Eoh} when is_binary(Eoh) ->
      log([check_header_eoh, Eoh]),
      check_header(Socket, [{"_EOH_", binary_to_list(Eoh)} | Headers], Mod);
    {error, timeout} ->
      case webtekcos_tools:check_version(Headers) of
        undef ->
          log(invalid_websocket_protocol),
          gen_tcp:close(Socket),
          exit(normal);
        VersionMod -> %% webtekcos_<version> module
          log([websocket_protocol, VersionMod]),
          VersionMod:handshake(Socket, Headers),
          log([handshake_end, VersionMod]),
          %% Set packet back to raw for the rest of the connection
          inet:setopts(Socket, [{packet, raw}, {active, true}]),
          loop(VersionMod, Socket, Mod, undef)
      end;
    {error, Error} ->
      log([check_header_error, Error]),
      gen_tcp:close(Socket),
      exit(normal)
  end.

loop(VersionMod, Socket, Mod, undef) ->
  LoopData = Mod:connect(),
  loop(VersionMod, Socket, Mod, LoopData);

loop(VersionMod, Socket, Mod, LoopData) ->
  NewLoopData = receive
    close ->
      Mod:disconnect(LoopData),
      gen_tcp:close(Socket),
      exit(normal);
    {tcp, Socket, Bin} ->
      case VersionMod:decode(Bin) of
        tcp_closed ->
          Mod:disconnect(LoopData),
          gen_tcp:close(Socket),
          exit(normal);
        Data ->
          Mod:handle_data(Data, LoopData)
      end;
    {send, Bin} when is_binary(Bin) ->
      gen_tcp:send(Socket, VersionMod:encode(Bin)), 
      LoopData;
    {tcp_closed, _} ->
      gen_tcp:close(Socket),
      Mod:disconnect(LoopData),
      exit(normal);
    Msg ->
      Mod:handle_message(Msg, LoopData)
  end,
  loop(VersionMod, Socket, Mod, NewLoopData).

close() ->
  self() ! close.

send_data(Data) when is_binary(Data) ->
  self() ! {send, Data}.

%%%
%%% private
%%%

loop(Socket, Mod) ->
  handshake(Socket, Mod).

log(Msg) ->
  io:format("===> ~p~n", [Msg]).
