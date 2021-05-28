#!/bin/bash
# This script does the following:
# restbatch start:
#  * Start a restbatch server (configuration: /etc/default/restbatch.conf)
# restbatch stop:
#  * Undo start

if [ -z ${RSCRIPT_PATH+x} ];
then
  RSCRIPT_PATH=$(which Rscript)
fi

function start {
  $RSCRIPT_PATH --no-save -e ".Last.value=yaml::read_yaml('$RESTBATCH_SETTINGS');restbatch:::start_server_internal(host=.Last.value\$host, port=.Last.value\$port, settings = ..sf)"
}

function stop {
  $RSCRIPT_PATH --no-save -e ".Last.value=yaml::read_yaml('$RESTBATCH_SETTINGS');restbatch::stop_server(host = .Last.value\$host,port = .Last.value\$port)"
}

function monitor {
  $RSCRIPT_PATH --no-save -e "source(system.file('dashboards/client/app.R', package = 'restbatch'))"
}


function usage {
    echo "Usage:"
    echo "   restbatch start - start restbatch"
    echo "   restbatch stop  - stop restbatch"
    echo "   restbatch monitor - R-shiny monitor"
}

if [ "$1" = "start" ]; then
    start
fi

if [ "$1" = "stop" ]; then
    stop
fi

if [ "$1" = "monitor" ]; then
    monitor
fi

if [ "$1" = "" ]; then
    usage
fi
