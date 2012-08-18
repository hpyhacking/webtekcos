-module(webtekcos).
-export([start_link/0, start_link/2, start_link/3, stop/1, loop/1]).
-export([send_data/1, send_data/2, close/0]).
-export([behaviour_info/1]).

start_link() ->
  start_link(?MODULE, "127.0.0.1", 3008).

start_link(Host, Port) when is_list(Host), is_integer(Port) ->
  start_link(?MODULE, Host, Port).
  
start_link(Name, Host, Port) when is_list(Host), is_integer(Port) ->
  Options = [{ip, Host}, {loop, fun ?MODULE:loop/1}, {port, Port}, {name, Name}],
  mochiweb_socket_server:start_link(Options).

stop(Name) ->
  mochiweb_socket_server:stop(Name).

behaviour_info(callbacks) ->
  [{connect, 0},
   {disconnect, 1},
   {handle_message, 2},
   {handle_data, 2}];

behaviour_info(_Other) ->
  undefined.

%%%
%%% websocket protocol
%%%

check_header(Socket, Headers) ->
  case gen_tcp:recv(Socket, 0, 50) of
    {ok, {http_header, _, Name, _, Value}} when is_atom(Name) ->
      log([fixed_header, Name, Value]),
      check_header(Socket, [{atom_to_list(Name), Value} | Headers]);
    {ok, {http_header, _, Name, _, Value}} ->
      log([check_header, Name, Value]),
      check_header(Socket, [{Name, Value} | Headers]);
    {ok, http_eoh} -> %% Check hixie76 last 8 byte checksum
      inet:setopts(Socket, [{packet, raw}]),
      check_header(Socket, Headers);
    {ok, Eoh} when is_binary(Eoh) ->
      log([check_header_eoh, Eoh]),
      check_header(Socket, [{"_EOH_", binary_to_list(Eoh)} | Headers]);
    {error, timeout} ->
      VersionMod = webtekcos_tools:check_version(Headers),
      handshake(VersionMod, Socket, Headers);
    {error, Error} ->
      log([check_header_error, Error]),
      gen_tcp:close(Socket),
      exit(normal)
  end.

handshake(undef, Socket, _Headers) ->
  log(invalid_websocket_protocol),
  gen_tcp:close(Socket),
  exit(normal);
handshake(VersionMod, Socket, Headers) ->
  log([websocket_protocol, VersionMod]),
  VersionMod:handshake(Socket, Headers),
  log([handshake_end, VersionMod]),

  Path = webtekcos_tools:get_header(Headers, "_PATH_"),
  %% convert query path to end of "handler" mod name
  Mod = list_to_atom(string:join(string:tokens(Path, "/") ++ ["handler"], "_")),
  check_mod(Mod, VersionMod, Socket, undef).

check_mod(Mod, VersionMod, Socket, LoopData) ->
  case code:is_loaded(Mod) of
    {file, _} ->
      inet:setopts(Socket, [{packet, raw}, {active, true}]),
      loop(VersionMod, Socket, Mod, LoopData);
    false ->
      case code:load_file(Mod) of
        {module, Mod} ->
          check_mod(Mod, VersionMod, Socket, LoopData);
        _ ->
          log([invalid_handle_mod, Mod]),
          gen_tcp:close(Socket),
          exit(normal)
      end
  end.

loop(VersionMod, Socket, Mod, undef) ->
  webtekcos_event:connect(),
  LoopData = Mod:connect(),
  loop(VersionMod, Socket, Mod, LoopData);

loop(VersionMod, Socket, Mod, LoopData) ->
  NewLoopData = receive
    close ->
      webtekcos_event:disconnect(),
      Mod:disconnect(LoopData),
      gen_tcp:close(Socket),
      exit(normal);
    {tcp, Socket, Bin} ->
      case VersionMod:decode(Bin) of
        tcp_closed ->
          webtekcos_event:disconnect(),
          Mod:disconnect(LoopData),
          gen_tcp:close(Socket),
          exit(normal);
        Data ->
          webtekcos_event:rcv_data(Data),
          Mod:handle_data(Data, LoopData)
      end;
    {send, Bin} when is_binary(Bin) ->
      webtekcos_event:send_data(Bin),
      gen_tcp:send(Socket, VersionMod:encode(Bin)), 
      LoopData;
    {tcp_closed, _} ->
      gen_tcp:close(Socket),
      webtekcos_event:disconnect(),
      Mod:disconnect(LoopData),
      exit(normal);
    Msg ->
      Mod:handle_message(Msg, LoopData)
  end,
  loop(VersionMod, Socket, Mod, NewLoopData).

close() ->
  self() ! close.

send_data(Data) -> send_data(self(), Data).
send_data(PID, Data) when is_pid(PID), is_binary(Data) ->
  PID ! {send, Data}.

%%%
%%% private
%%%

loop(Socket) ->
  inet:setopts(Socket, [{packet, http}]),
  case gen_tcp:recv(Socket, 0) of
    {ok, {http_request, _Method, {abs_path, Path}, _Version}} ->
      check_header(Socket, [{"_PATH_", Path}]);
    _ ->
      log(invalid_handshake_request)
  end.

log(_Msg) -> ok.
