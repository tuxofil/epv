#!/bin/sh

##
## Initiate Erlang remote shell to the running epv instance
##

exec su -l -s /bin/bash -c 'erl -name r@127.1 -remsh epv@127.1' epv
