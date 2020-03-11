-module(transceiver_mqtt).

-behaviour(gen_server).

%API
-export([start_link/1]).

%gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(TOPIC_SUB_REGISTER, application:get_env(transceiver, topic_sub_register)).
-define(TOPIC_SUB_EVENT, application:get_env(transceiver, topic_sub_event)).
-define(TOPIC_SUB_CON, application:get_env(transceiver, topic_sub_con)).
-define(START_OPT_MQTT, application:get_env(transceiver, start_options_mqtt)).
-define(QOS, application:get_env(transceiver, qos)).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_) ->
    State = self(),
    self() ! first_subscribe,
    {ok, State}.

handle_call(Request, From, State) ->

    ok = io:format("~nRequest: ~p~nFrom: ~p~nSate: ~p~n~n",
        [Request, From, State]
    ),

    {reply, reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(first_subscribe, _State) ->
    {ok, TopicSubRegister} = ?TOPIC_SUB_REGISTER,
    {ok, TopicSubEvent} = ?TOPIC_SUB_EVENT,
    {ok, TopicSubCon} = ?TOPIC_SUB_CON,
    Topics = [TopicSubRegister, TopicSubEvent, TopicSubCon],
    NewState = subscribe_mqtt(Topics),
    {noreply, NewState};
handle_info({publish, MsgMap}, State) ->
    Payload = maps:get(payload, MsgMap),
    MsgTerm = binary_to_term(Payload),

    ok = io:format("~n~nMsgMap: ~p~n", [MsgMap]),
    ok = io:format("~nMsgTerm: ~p~n~n", [MsgTerm]),

    {noreply, State};
handle_info(Info, State) ->

    ok = io:format("~nInfo: ~p~n", [Info]),

    {noreply, State}.

subscribe_mqtt(Topics) ->
    [TopicRegister, TopicEvent, TopicCon] = Topics,
    {ok, StartOptions} = ?START_OPT_MQTT,
    {ok, ConnPid} = emqtt:start_link(StartOptions),
    {ok, _Props} = emqtt:connect(ConnPid),
    {ok, QoS} = ?QOS,
    {ok, _Props, _ReasonCodes} = emqtt:subscribe(ConnPid, {TopicRegister, QoS}),
    {ok, _Props2, _ReasonCodes2} = emqtt:subscribe(ConnPid, {TopicEvent, QoS}),
    {ok, _Props3, _ReasonCodes3} = emqtt:subscribe(ConnPid, {TopicCon, QoS}).
