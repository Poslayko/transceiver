-module(transceiver_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
        #{
            id => transceiver_mqtt,
            start => {transceiver_mqtt, start_link, [arg]},
            restart => transient
        }
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
