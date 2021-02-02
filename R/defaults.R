# Defalts

#' @export
default_host <- function(host){
  if(!missing(host)){
    options("restbenchclient.server_host" = host)
  }
  getOption("restbenchclient.server_host", '127.0.0.1')
}

#' @export
default_port <- function(port){
  if(!missing(port)){
    port <- as.integer(port)
    options("restbenchclient.server_port" = port)
  }
  getOption("restbenchclient.server_port", 7033)
}

#' @export
default_protocol <- function(protocol = c('', 'http', 'https')){
  protocol <- match.arg(protocol)
  if(protocol != ''){
    options("restbench.protocol" = protocol)
  }
  getOption("restbench.protocol", 'http')
}

