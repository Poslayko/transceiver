#!/bin/sh

erl -pa $PWD/ebin/ -pa $PWD/deps/*/ebin/ -run transceiver -sname transceiver -config env/dev
