#!/bin/sh
### BEGIN INIT INFO
# Provides:          epv
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Erlang Photo Viewer
# Description:       Web based photo gallery written on Erlang
### END INIT INFO

# Author: Aleksey Morarash <aleksey.morarash@gmail.com>

NAME=epv
RUN_AS_USER="$NAME"
CONFIG=/etc/"$NAME".config
LOGDIR=/var/log/"$NAME"
ERL=/usr/bin/erl

prepare(){
    set -e
    [ -d "$LOGDIR" ] || \
        install --mode=755 --owner="$RUN_AS_USER" --directory "$LOGDIR"
    HOMEDIR=`rundir`
    [ -d "$HOMEDIR" ] || \
        install --mode=755 --owner="$RUN_AS_USER" --directory "$HOMEDIR"
    COOKIE_FILE="$HOMEDIR"/.erlang.cookie
    [ -f "$COOKIE_FILE" ] || create_cookie_file "$COOKIE_FILE"
    set +e
}

create_cookie_file(){
    local COOKIE_FILE="$1"
    touch "$COOKIE_FILE" && \
    chown "$RUN_AS_USER": "$COOKIE_FILE" && \
    chmod 0600 "$COOKIE_FILE" && \
    cat /dev/urandom | tr --delete --complement a-zA-Z0-9 | \
        head --bytes=20 > "$COOKIE_FILE"
}

rundir(){
    getent passwd "$RUN_AS_USER" | cut --delimiter=":" --fields=6
}

start(){
    if status; then
        echo "$NAME already started"
        return 0
    fi
    set -e
    echo -n "Starting $NAME..."
    su_exec "$ERL \
        -name `nodename` \
        -boot start_sasl \
        -detached \
        -config $CONFIG \
        -s epv start_permanent"
    echo "done"
    set +e
}

stop(){
    if ! status; then
        echo "$NAME already stopped"
        return 0
    fi
    echo -n "Stopping $NAME..."
    su_exec "$ERL \
        -name `randname` \
        -noinput \
        -hidden \
        -s epv stop `nodename`"
    RET=$?
    if [ "$RET" = "0" ]; then
        echo "stopped"
    else
        echo "failed"
    fi
    return "$RET"
}

status(){
    su_exec "$ERL \
        -name `randname` \
        -noinput \
        -hidden \
        -s epv ping `nodename`"
}

wait_to_stop(){
    while status; do :; done
}

attach(){
    if ! status; then
        echo "$NAME is stopped. No node to attach."
        return 1
    fi
    su_exec "$ERL \
        -name `randname` \
        -hidden \
        -remsh `nodename`"
}

hup(){
    if ! status; then
        echo "$NAME is stopped. Nothing to do."
        return 1
    fi
    su_exec "$ERL \
        -name `randname` \
        -noinput \
        -hidden \
        -s epv hup `nodename`"
}

su_exec(){
    COMMAND="$1"
    if [ "$RUN_AS_USER" = "root" ]; then
        sh -c "$COMMAND"
    else
        sudo -n -u "$RUN_AS_USER" -i $COMMAND
    fi
}

nodename(){
    echo "${NAME}@127.0.0.1"
}

randname(){
    RANDOM=`date +%N`
    echo "${NAME}_${RANDOM}_remsh@127.0.0.1"
}

prepare

case "$1" in
    start)
        start
        ;;

    stop)
        stop
        ;;

    restart)
        if status; then
            stop
            wait_to_stop
        fi
        start
        ;;

    status)
        if status; then
            echo "$NAME is running"
            exit 0
        else
            echo "$NAME is not running"
            exit 1
        fi
        ;;

    attach)
        attach
        ;;

    hup)
        hup
        ;;

    *)
        echo "Usage: $0 {start|stop|restart|status|attach|hup}"
        exit 1
        ;;
esac

