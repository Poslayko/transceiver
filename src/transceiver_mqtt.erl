-module(transceiver_mqtt).

-behaviour(gen_server).

%API
-export([start_link/1]).
-export([send_message_mqtt/1]).
-export([pipeline_online/0]).
-export([pipeline_offline/0]).
-export([send_msg/2]).

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

send_msg(Topic, DataMsg) ->
    send_message_mqtt({send_msg, Topic, DataMsg}).

send_message_mqtt(Msg) ->
    ok = gen_server:cast(?MODULE, Msg),
    ok.

pipeline_online() ->
    send_message_mqtt({send_msg_mqtt, <<"connection/pipeline">>, <<"online">>}).

pipeline_offline() ->
    send_message_mqtt({send_msg_mqtt, <<"connection/pipeline">>, <<"offline">>}).

init(_) ->
    State = self(),
    self() ! first_subscribe,
    {ok, State}.

handle_call(Request, From, State) ->

    ok = io:format("~nRequest: ~p~nFrom: ~p~nSate: ~p~n~n",
        [Request, From, State]
    ),

    {reply, reply, State}.

handle_cast({send_msg_mqtt, Topic, Msg}, State) ->
    ConnPid = maps:get(conn_pid, State),
    TermMsg = term_to_binary(Msg),
    {ok, QoS} = ?QOS,
    ok = emqtt:publish(ConnPid, Topic, TermMsg, [QoS, {retain, true}]),
    {noreply, State};
handle_cast({send_msg, Topic, Msg}, State) ->
    ConnPid = maps:get(conn_pid, State),
    TermMsg = term_to_binary(Msg),
    {ok, QoS} = ?QOS,
    {ok, _Pr} = emqtt:publish(ConnPid, Topic, TermMsg, QoS),
    {noreply, State};
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
    Topic = maps:get(topic, MsgMap),

    ok = io:format("~n~nMsgMap: ~p~n", [MsgMap]),
    ok = io:format("~nMsgTerm: ~p~n~n", [MsgTerm]),

    MainResponse = case is_map(MsgTerm) of
        true ->
            case maps:is_key(event_data, MsgTerm) of
                true ->
                    Msg = maps:get(event_data, MsgTerm),
                    case Msg of
                        {ok, {_, _, ResponseBin}} ->
                            jsx:decode(ResponseBin);
                        Msg ->
                            Msg
                    end;
                false ->
                    MsgTerm
            end;
        false ->
            MsgTerm
    end,

    ok = io:format("Topic: ~p~n", [Topic]),
    ok = io:format("MainResponse: ~p~n", [MainResponse]),

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
    {ok, _Props3, _ReasonCodes3} = emqtt:subscribe(ConnPid, {TopicCon, QoS}),
    #{conn_pid => ConnPid}.
