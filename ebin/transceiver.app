{application, 'transceiver', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['transceiver','transceiver_app','transceiver_mqtt','transceiver_sup']},
	{registered, [transceiver_sup]},
	{applications, [kernel,stdlib,emqtt]},
	{mod, {transceiver_app, []}},
	{env, []}
]}.