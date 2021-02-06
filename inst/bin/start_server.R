library(restbatch)

..s <- yaml::read_yaml("settings.yaml")
restbatch:::start_server_internal(host=..s$host,port=as.integer(..s$port),settings='settings.yaml')


