#!/bin/sh
erl -node toolnode -config mongodbdriver.config -name tool@127.0.0.1 -pa ./ebin -pa ./deps/*/ebin -eval "mongodbdriver_sup:start_link()"
