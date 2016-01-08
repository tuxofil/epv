#!/bin/sh -e

. /etc/default/epv

## Erlang emulator can't start without HOME environment variable.
## So, to start EPV as system service we can't start it with
## 'su' utility without the '--login' option.
## To avoid using 'su --login' we set all vital environment here
## and then start the EPV.
##
## Another reason to set HOME is the Erlang Distribution. When
## started, it tries to create a '.erlang.cookie' file in the
## directory pointed by HOME environment variable. If it fail,
## whole startup process will fail. Sad but true. Thats why we
## cannot set HOME to '/'.

export HOME="$CACHE_PATH"

# Generate command line for the daemon
ARGS="$MEDIA_PATH $CACHE_PATH"
[ -n "$LANGUAGE" ] && ARGS="-l $LANGUAGE $ARGS"
[ -n "$BIND_ADDR" ] && ARGS="-i $BIND_ADDR $ARGS"
[ -n "$BIND_PORT" ] && ARGS="-p $BIND_PORT $ARGS"
[ "$SHOW_TAGS" = "yes" -o "$SHOW_TAGS" = "1" -o "$SHOW_TAGS" = "true" ] || \
    ARGS="--no-tags $ARGS"
[ -n "$LOG_PATH" ] && ARGS="--log $LOG_PATH $ARGS"
[ -n "$LOGLEVEL" ] && ARGS="-v $LOGLEVEL $ARGS"

exec /usr/bin/epv $ARGS
