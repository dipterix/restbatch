# Defalts

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

#' @export
default_port <- function(port){
  if(!missing(port)){
    port <- as.integer(port)
    options("restbatchclient.server_port" = port)
  }
  getOption("restbatchclient.server_port", 7033)
}

#' @export
default_protocol <- function(protocol = c('', 'http', 'https')){
  protocol <- match.arg(protocol)
  if(protocol != ''){
    options("restbatch.protocol" = protocol)
  }
  getOption("restbatch.protocol", 'http')
}

