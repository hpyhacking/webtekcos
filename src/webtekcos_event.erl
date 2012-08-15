-module(webtekcos_event).
-export([start_link/0, add_handler/2, delete_handler/2]).

-export([connect/0, disconnect/0, start/3]).
-export([rcv_data/1, send_data/1]).

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

start(Name, Host, Port) ->
  gen_event:notify(?MODULE, {start, {Name, Host, Port}}).

rcv_data(Data) ->
  gen_event:notify(?MODULE, {rcv_data, Data}).

send_data(Data) ->
  gen_event:notify(?MODULE, {send_data, Data}).
