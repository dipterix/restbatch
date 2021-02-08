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




get_ip <- function(get_public = NA, timeout = 3){
  ip <- list(
    available = c('127.0.0.1', '0.0.0.0'),
    public = if(isFALSE(get_public)) { NULL } else { getOption("restbench.public_ip", NULL) }
  )
  try({
    s <- switch (
      get_os(),
      'windows' = {
        s <- system("ipconfig", intern=TRUE)
        s <- stringr::str_extract(s, "IPv4 Address.*[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}.*")
        s <- s[!is.na(s)]
        stringr::str_extract(s, '[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}')
      },
      'darwin' = {
        s <- system("ifconfig 2>&1", intern = TRUE)
        s <- stringr::str_extract(s, "inet.*[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")
        s <- s[!is.na(s)]
        # extract the first one as the second is mask
        stringr::str_extract(s, '[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}')
      }, {
        s <- system("ip addr", intern = TRUE)
        s <- stringr::str_extract(s, "inet.*[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")
        s <- s[!is.na(s)]
        # extract the first one as the second is mask
        stringr::str_extract(s, '[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}')
      }
    )
    ip$available <- c(ip$available, s[!is.na(s)])
  }, silent = TRUE)

  # also use ipify
  if(isTRUE(get_public)){
    ip$public <- getOption("restbench.public_ip", try({
      res <- httr::GET("https://api.ipify.org?format=json", httr::timeout(timeout))
      res <- httr::content(res, encoding = 'UTF-8')
      s <- res$ip
      s <- stringr::str_extract(s, "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")
      s <- s[!is.na(s)]
      options("restbench.public_ip" = s)
      s
    }, silent = TRUE))
  }

  ip$available <- unique(ip$available)
  ip
}

host_is_local <- function(host, include_public = NA, ...){
  host %in% unlist(get_ip(get_public = include_public, ...))
}


portAvailable <- function(port){
  tryCatch({
    srv <- httpuv::startServer(
      host = default_host(allow0 = FALSE),
      port, list(), quiet = TRUE)
  }, error = function(e) {
    port <<- 0
  }
  )
  if (port != 0) {
    httpuv::stopServer(srv)
  }
  port != 0
}

findPort <- function (port, mustWork = NA) {
  if (missing(port) || is.null(port)) {
    port <- as.integer(restbatch_getopt("default_port", default = 7033))
    if(length(port) != 1 || is.na(port) || !is.integer(port)){
      port <- httpuv::randomPort()
    }
    for (i in 1:11) {
      if(!portAvailable(port)){
        port <- httpuv::randomPort()
      } else {
        restbatch_setopt('default_port', port, .save = FALSE)
        break
      }
    }
  }
  if (port == 0) {
    msg <- "Unable to start a server. Either the port specified was unavailable or we were unable to find a free port."
    if(isTRUE(mustWork)){
      stop(msg)
    } else if(is.na(mustWork)){
      warning(msg)
    }
  }
  port
}

