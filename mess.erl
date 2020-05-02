-module(mess).
-behaviour(application).

-export([start/2, stop/1]).

start(_,_) -> mess_server:start_server().

stop(_) -> exit(whereis(mess_server),kill).
