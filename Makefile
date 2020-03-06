PROJECT = transceiver
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = emqtt

dep_emqtt = git https://github.com/emqx/emqtt.git master

include erlang.mk
