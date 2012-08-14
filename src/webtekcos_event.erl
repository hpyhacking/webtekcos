-module(webtekcos_event).
-export([start_link/0, add_handler/2, delete_handler/2]).

start_link() ->
  gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
  gen_event:delete_handler(?MODULE, Handler, Args).

connect() ->
  gen_event:notify(?MODULE, connect).

disconnect() ->
  gen_event:notify(?MODULE, disconnect).
