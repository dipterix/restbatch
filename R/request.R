prepare_request <- function(){
  uid <- get_user()
  keys <- private_key(uid)

  time <- get_timeStamp()
  tokens <- sapply(keys, function(key){
    encrypt_string(time, key = key)
  })
  dipsaus::list_to_fastmap2(list(
    userid = uid,
    username = get_username(),
    tokens = tokens,
    timeStamp = time
  ))
}

#' Send requests to the server to query the server status or task status
#' @description \describe{
#' \item{\code{request_server}}{sends general request with authentication
#' tokens ready in the request headers}
#' \item{\code{request_task_list}}{get list of tasks of your current userid
#' from the server}
#' }
#' @param url,protocol,host,port server location-related configurations
#' @param body a list of request body
#' @param header additional header key-value pairs
#' @param method method of request; choices are \code{'POST'} and \code{'GET'}
#' @param encode serialization method to encode request body
#' @param task_status task status to filter
#' @param path route path to send requests to
#' @return \code{'httr'} response. You can use \code{\link[httr]{content}} to
#' check the response contents.
#' @name restbatch-requests
NULL

#' @rdname restbatch-requests
#' @export
request_server <- function(
  url, body = list(), header = list(), method = c('POST', 'GET'), encode = 'json'){
  method <- match.arg(method)
  conf <- prepare_request()
  dipsaus::list_to_fastmap2(header, conf)
  conf <- as.list(conf)
  names(conf) <- sprintf("restbatch.%s", names(conf))

  if(method == 'POST'){
    f <- httr::POST
  }else{
    f <- httr::GET
  }
  tryCatch({
    res <- f(
      url = url,
      config = do.call(httr::add_headers, conf),
      encode = encode,
      body = body
    )
  }, error = function(e){
    e$message <- sprintf("%s\n  Please make sure the server is on and you are using the right protocol.\n", e$message)
    stop(e)
  })
  res

}

#' @rdname restbatch-requests
#' @export
request_task_list <- function(task_status = 'valid', host = default_host(allow0 = FALSE),
                              port = default_port(), protocol = default_protocol(), path = 'jobs/list'){
  url <- sprintf('%s://%s:%d/%s', protocol, host, port, path)
  res <- request_server(url, body = list(status = task_status), method = 'POST')
  content <- httr::content(res)

  content <- do.call('rbind', lapply(content, function(item){
    as.data.frame(item)
  }))

  res <- data.frame(
    name = as.character(content$name),
    status = as.character(factor(content$status, levels = c(0,1,2,-1), labels = c("init", "running", "finish", "canceled"))),
    error = as.logical(content$error),
    packed = as.logical(content$packed),
    time_added = as.POSIXct(content$time_added, origin="1970-01-01"),
    stringsAsFactors = FALSE
  )
  attr(res, 'userid') <- get_user()
  res
}

