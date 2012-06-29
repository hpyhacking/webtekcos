-module(webtekcos_tools).
-export([check_version/1, get_header/2]).

check_version(Headers) ->
  case string:to_lower(get_header(Headers, "Upgrade", "undef")) of
    "websocket" ->
      check_version([{"Sec-WebSocket-Key1", webtekcos_hixie76}, {"Sec-Websocket-Key", webtekcos_rfc6455}], Headers);
    _ -> 
      undef
  end.

get_header(Headers, Key, Def) ->
  case get_header(Headers, Key) of
    undef -> Def;
    Val -> Val
  end.

get_header([{K, V}|T], Key) when is_list(K) ->
  case string:to_lower(K) == string:to_lower(Key) of
    true -> V;
    _ -> 
      get_header(T, Key)
  end;
get_header([], _FindKey) ->
  undef.

check_version([], _Headers) -> undef;
check_version([{K, V}|T], Headers) ->
  case get_header(Headers, K) of
    undef -> check_version(T, Headers);
    _ -> V
  end.
