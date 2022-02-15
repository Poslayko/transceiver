PROJECT = transceiver
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = emqtt jsx

dep_emqtt = git https://github.com/emqx/emqtt.git            v1.0.0
dep_jsx   = git https://github.com/talentdeficit/jsx.git     v3.0.0

CONFIG ?= dev
RELX_OPTS = -o _rel/$(CONFIG) --sys_config $(CURDIR)/config/$(CONFIG).config

include erlang.mk
