
##How to send message with emqttd

Token = "any_token",
BindId = <<"abcdef">>,
Parameters = #{token => Token, bind_id => BindId},
MethodName = <<"get_personal_client_info">>,
DataMsg = #{method_name => MethodName, parameters => Parameters},
Topic = <<"monobank_test">>.
transceiver_mqtt:send_msg(Topic, DataMsg).

