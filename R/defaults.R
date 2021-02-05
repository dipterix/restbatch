# Defalts

#' Get or set default host, port, and protocols
#' @param host 'IPv4' address
#' @param port an integer of port to expose, ranging from 1 to 65535
#' @param protocol either 'http' or 'https'
#' @param allow0 internally used
#' @return The current default options
#' @details By default, servers will run on local host ('127.0.0.1') at port
#' '7033' with protocol 'http'. The tasks will be sent to this address too.
#' @name default_url
NULL

#' @rdname default_url
#' @export
default_host <- function(host, allow0 = TRUE){
  if(!missing(host)){
    options("restbatchclient.server_host" = host)
  }
  ret <- getOption("restbatchclient.server_host", '127.0.0.1')
  if(!allow0 && ret == '0.0.0.0'){
    ret <- '127.0.0.1'
  }
  ret
}

#' @rdname default_url
#' @export
default_port <- function(port){
  if(!missing(port)){
    port <- as.integer(port)
    options("restbatchclient.server_port" = port)
  }
  getOption("restbatchclient.server_port", 7033)
}

#' @rdname default_url
#' @export
default_protocol <- function(protocol = c('', 'http', 'https')){
  protocol <- match.arg(protocol)
  if(protocol != ''){
    options("restbatch.protocol" = protocol)
  }
  getOption("restbatch.protocol", 'http')
}

