#!/usr/bin/env bash

settings=$1
port=$2
host=$3
pcol=$4
rhome=$5

if [ -z "$port" ]
then
  port=7033
fi
if [ -z "$host" ]
then
  host="127.0.0.1"
fi
if [ -z "$pcol" ]
then
  pcol="http"
fi

echo "Starting $pcol://$host:$port with settings:"
echo
echo "$settings"
cat "$settings"
echo

# Run R command
if [ -z "$rhome" ]
then
  rhome=$(which R)
fi

temp_file=$(dirname "$0")
echo "restbench:::start_server_internal(host='$host',port=$port,settings='$settings')" > "$temp_file/start_server.R"

#
nohup "$rhome" CMD BATCH --no-save --no-restore "$temp_file/start_server.R" "$temp_file/server.log" & disown -a

exit

