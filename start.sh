#!/bin/sh

exec erl \
    -boot start_sasl \
    -config etc/app.config \
    -pa ebin \
    -s epv

