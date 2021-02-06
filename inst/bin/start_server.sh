#!/bin/bash

rhome=$1

if [ -z "$rhome" ]
then
  rhome=$(which R)
fi

temp_file=$(dirname "$0")
cd "$temp_file"

# if [ ! -f ./settings.yaml ]; then
#   echo "Settings.yaml not found. Use system default"
#   conf=$(Rscript --vanilla -e "cat(system.file('default_settings.yaml', package = 'restbatch'))")
#   cp "$conf" ./default_settings.yaml
# else
#   echo "Use local 'settings.yaml':"
# fi

nohup "$rhome" CMD BATCH --no-save --no-restore "$temp_file/start_server.R" "$temp_file/server.log" & disown -a

exit

