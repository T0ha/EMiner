#!/bin/sh
rebar get-deps compile &&
erl -pa ./ebin -pa ./deps/*/ebin -config priv/eminer -eval "application:start(eminer)" #-run observer
