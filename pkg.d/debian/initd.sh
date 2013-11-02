#!/bin/sh
### BEGIN INIT INFO
# Provides:          epv
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Erlang Photo Viewer
# Description:       EPV is lightweight and minimalistic Web-gallery
#                    for your photographs and videos.
### END INIT INFO

# Author: Aleksey Morarash <aleksey.morarash@gmail.com>

PATH=/bin:/sbin:/usr/bin
DESC="Erlang Photo Viewer"
NAME=epv
DAEMON=/usr/bin/$NAME
PIDFILE=/var/run/$NAME.pid
SCRIPTNAME=/etc/init.d/$NAME

# Exit if the package is not installed
[ -x "$DAEMON" ] || exit 0

# Read configuration variable file if it is present
[ -r /etc/default/$NAME ] && . /etc/default/$NAME

DAEMON_ARGS="$MEDIA_PATH $CACHE_PATH"
[ -n "$LANGUAGE" ] && DAEMON_ARGS="-l $LANGUAGE $DAEMON_ARGS"
[ -n "$BIND_ADDR" ] && DAEMON_ARGS="-i $BIND_ADDR $DAEMON_ARGS"
[ -n "$BIND_PORT" ] && DAEMON_ARGS="-p $BIND_PORT $DAEMON_ARGS"
[ "$SHOW_TAGS" = "yes" -o "$SHOW_TAGS" = "1" -o "$SHOW_TAGS" = "true" ] || \
    DAEMON_ARGS="--no-tags $DAEMON_ARGS"
[ -n "$LOG_PATH" ] && DAEMON_ARGS="--log $LOG_PATH $DAEMON_ARGS"
[ -n "$LOGLEVEL" ] && DAEMON_ARGS="-v $LOGLEVEL $DAEMON_ARGS"

[ -r /lib/init/vars.sh ] && . /lib/init/vars.sh
. /lib/lsb/init-functions

do_start() {
    # Return
    #   0 if daemon has been started
    #   1 if daemon was already running
    #   2 if daemon could not be started
    start-stop-daemon --quiet --status \
        --pidfile $PIDFILE --user $NAME && return 1
    start-stop-daemon --quiet --start --background --make-pidfile \
        --pidfile $PIDFILE --user $NAME --chuid $NAME \
        --exec $DAEMON -- $DAEMON_ARGS || return 2
}

do_stop() {
    # Return
    #   0 if daemon has been stopped
    #   1 if daemon was already stopped
    #   2 if daemon could not be stopped
    #   other if a failure occurred
    start-stop-daemon --stop --quiet --retry=TERM/30/KILL/5 \
        --pidfile $PIDFILE --user $NAME
    RETVAL="$?"
    [ "$RETVAL" = 2 ] && return 2
    rm -f $PIDFILE
    return "$RETVAL"
}

case "$1" in
    start)
        [ "$VERBOSE" != no ] && log_daemon_msg "Starting $DESC" "$NAME"
        do_start
        case "$?" in
            0|1) [ "$VERBOSE" != no ] && log_end_msg 0 ;;
            2) [ "$VERBOSE" != no ] && log_end_msg 1 ;;
        esac
        ;;
    stop)
        [ "$VERBOSE" != no ] && log_daemon_msg "Stopping $DESC" "$NAME"
        do_stop
        case "$?" in
            0|1) [ "$VERBOSE" != no ] && log_end_msg 0 ;;
            2) [ "$VERBOSE" != no ] && log_end_msg 1 ;;
        esac
        ;;
    status)
        status_of_proc "$DAEMON" "$NAME" && exit 0 || exit $?
        ;;
    restart|force-reload)
        log_daemon_msg "Restarting $DESC" "$NAME"
        do_stop
        case "$?" in
            0|1)
                do_start
                case "$?" in
                    0) log_end_msg 0 ;;
                    1) log_end_msg 1 ;; # Old process is still running
                    *) log_end_msg 1 ;; # Failed to start
                esac
                ;;
            *)
                # Failed to stop
                log_end_msg 1
                ;;
        esac
        ;;
    *)
        echo "Usage: $SCRIPTNAME {start|stop|status|restart|force-reload}" >&2
        exit 3
        ;;
esac

:
