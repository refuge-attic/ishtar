#!/bin/sh
erl -pa ebin -pa deps/*/ebin -s proxy_openbsd
