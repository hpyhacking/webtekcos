-module(webtekcos_websocket_server).
-export([start_link/0, start_link/3, start_link/4, stop/1, loop/2]).

start_link() ->
  start_link(?MODULE, "127.0.0.1", 3008, fun easy_loop/2).

start_link(Host, Port, Loop) when is_list(Host), is_integer(Port) ->
  start_link(?MODULE, Host, Port, Loop).
  
start_link(Name, Host, Port, Loop) when is_list(Host), is_integer(Port) ->
  Fun = fun (Socket) -> ?MODULE:loop(Socket, Loop) end,
  Options = [{ip, Host}, {loop, Fun}, {port, Port}, {name, Name}],
 io:format("start websocket server ...~n"),
 mochiweb_socket_server:start_link(Options).

stop(Name) ->
  mochiweb_socket_server:stop(Name).

%%%
%%% websocket protocol
%%%

handshake(Socket, Fun) ->
  inet:setopts(Socket, [{packet, http}]),
  %inet:setopts(Socket, [{packet, raw}]),
  case gen_tcp:recv(Socket, 0) of
    {ok, {http_request, _Method, {abs_path, Path}, _Version}} ->
      check_header(Socket, [{"_PATH_", Path}], Fun);
    _ ->
      log(invalid_handshake_request)
  end.

check_header(Socket, Headers, Fun) ->
  case gen_tcp:recv(Socket, 0, 50) of
    {ok, {http_header, _, Name, _, Value}} when is_atom(Name) ->
      log([fixed_header, Name, Value]),
      check_header(Socket, [{atom_to_list(Name), Value} | Headers], Fun);
    {ok, {http_header, _, Name, _, Value}} ->
      log([check_header, Name, Value]),
      check_header(Socket, [{Name, Value} | Headers], Fun);
    {ok, http_eoh} ->
      inet:setopts(Socket, [{packet, raw}]),
      check_header(Socket, Headers, Fun);
    {ok, Eoh} when is_binary(Eoh) ->
      log([check_header_eoh, Eoh]),
      check_header(Socket, [{"_EOH_", binary_to_list(Eoh)} | Headers], Fun);
    {error, timeout} ->
      case webtekcos_tools:check_version(Headers) of
        undef ->
          log(invalid_websocket_protocol),
          gen_tcp:close(Socket),
          exit(normal);
        Mod -> %% webtekcos_<version> module
          log([websocket_protocol, Mod]),
          Mod:handshake(Socket, Headers),
          log([handshake_end, Mod]),
          %% Set packet back to raw for the rest of the connection
          inet:setopts(Socket, [{packet, raw}, {active, true}]),
          loop(Mod, Socket, Fun, undef)
      end;
    {error, Error} ->
      log([check_header_error, Error]),
      gen_tcp:close(Socket),
      exit(normal)
  end.

loop(Mod, Socket, Fun, undef) ->
  LoopData = Fun(connected, undef),
  loop(Mod, Socket, Fun, LoopData);

loop(Mod, Socket, Fun, LoopData) ->
  NewLoopData = receive
    close ->
      gen_tcp:close(Socket),
      Fun(disconnected, LoopData),
      exit(normal);
    {tcp, Socket, Bin} ->
      case Mod:decode(Bin) of
        tcp_closed ->
          Fun(disconnected, LoopData),
          gen_tcp:close(Socket),
          exit(normal);
        Data ->
          Fun({recv, Data}, LoopData)
      end;
    {send, Bin} when is_binary(Bin) ->
      gen_tcp:send(Socket, Mod:encode(Bin)), 
      LoopData;
    {tcp_closed, _} ->
      gen_tcp:close(Socket),
      Fun(disconnected, LoopData),
      exit(normal);
    Msg ->
      log([msg, Msg]),
      Fun({msg, Msg}, LoopData)
  end,
  loop(Mod, Socket, Fun, NewLoopData).

%%%
%%% private
%%%

loop(Socket, Fun) ->
  handshake(Socket, Fun).

easy_loop(Msg, Data) ->
  error_logger:info_report([{msg, Msg}, {data, Data}]).

log(Msg) ->
  io:format("===> ~p~n", [Msg]).
