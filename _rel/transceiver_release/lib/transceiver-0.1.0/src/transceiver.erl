-module(transceiver).

-export([start/0]).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE).
