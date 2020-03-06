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
    NewState = [subscribe_mqtt(TopicSubRegister)],
    {noreply, NewState};
handle_info({publish, MsgMap}, State) ->
    Payload = maps:get(payload, MsgMap),
    MsgTerm = binary_to_term(Payload),

    ok = io:format("~nMsgTerm: ~p~n", [MsgTerm]),

    {noreply, State};
handle_info(Info, State) ->

    ok = io:format("~nInfo: ~p~n", [Info]),

    {noreply, State}.

subscribe_mqtt(TopicSub) ->
    {ok, StartOptions} = ?START_OPT_MQTT,
    {ok, ConnPid} = emqtt:start_link(StartOptions),
    {ok, _Props} = emqtt:connect(ConnPid),
    {ok, QoS} = ?QOS,
    {ok, _Props, _ReasonCodes} = emqtt:subscribe(ConnPid, {TopicSub, QoS}).
