prepare_request <- function(){
  uid <- get_user()
  # keys <- private_key(uid)

  time <- get_timeStamp()
  # tokens <- sapply(keys, function(key){
  #   encrypt_string(time, key = key)
  # })
  dipsaus::list_to_fastmap2(list(
    userid = uid,
    username = get_username(),
    # tokens = tokens,
    timeStamp = time
  ))
}

#' Send requests to the server to query the server status or task status
#' @description \describe{
#' \item{\code{request_server}}{sends general request with authentication
#' tokens ready in the request headers}
#' \item{\code{request_task_list}}{get list of tasks of your current user ID
#' (\code{userid}) from the server}
#' }
#' @param protocol,host,port,path server location-related configurations, the
#' 'url' of request will be \code{protocol://host:port/path}
#' @param body a list of request body
#' @param header additional header key-value pairs
#' @param method method of request; choices are \code{'POST'} and \code{'GET'}
#' @param encode serialization method to encode request body
#' @param timeout maximum waiting time in seconds before aborting the request;
#' default is 15. If the server is off or fails to respond before time running
#' out, the request will result in an error.
#' @param task_status task status to filter
#' @return \code{'httr'} response. You can use \code{\link[httr]{content}} to
#' check the response contents.
#' @name restbatch-requests
NULL

authtoken <- function(host, port, token){
  if(missing(token)){
    ans <- .globals$servers[[host]]
    if(!is.list(ans)){ return("") }
    ans <- ans[[port]]
    if(!is.list(ans)){ return("") }
    ans <- ans$token
    if(!length(ans)){ return("") }
    ans
  } else {
    .globals$servers[[host]] <- dipsaus::list_to_fastmap2(list(), .globals$servers[[host]])
    if(is.null(.globals$servers[[host]][[port]])){
      autoclose_server(host = host, port = port, auto_close = FALSE)
    }
    .globals$servers[[host]][[port]]$token <- token
    token
  }

}

#' @rdname restbatch-requests
#' @export
request_server <- function(
  path, host = default_host(), port = default_port(), protocol = default_protocol(),
  body = list(), header = list(), method = c('POST', 'GET'), encode = 'json', timeout = 15){
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

  request_url <- sprintf("%s://%s:%.0f/%s", protocol, host, port, path)
  # ping_url <- sprintf("%s://%s:%d/%s", protocol, host, port, 'validate/ping')

  # get previous answer
  token <- authtoken(host = host, port = port)

  tryCatch({
    # get authentication error
    res <- f(
      url = request_url,
      config = do.call(httr::add_headers, conf),
      httr::authenticate(get_user(), token, "basic"),
      httr::timeout(timeout),
      encode = encode,
      body = body
    )

    if(res$status_code == 403){
      stop("Authentication failed. You don't have access to the server.")
    }

    if(res$status_code == 401){
      # solve the auth problem
      prob <- httr::content(res)
      # verify the userid
      stopifnot(prob$userid == get_user())

      # get question
      question <- prob$question

      # get the original data
      server_time <- stringr::str_sub(question, end = 23)
      server_keymd5 <- stringr::str_sub(question, start = 25, end = 56)
      question <- stringr::str_sub(question, start = 58)
      question <- stringr::str_split_fixed(question, '[ ]+', 3)
      avail_md5s <- question[[1]]
      question <- question[[2]]

      # get my keys
      avail_md5s <- unlist(stringr::str_split(avail_md5s, ";"))
      my_keys <- private_key(prob$userid)
      is_key <- vapply(my_keys, function(key){
        md5 <- as.character(key$pubkey$fingerprint)
        md5 %in% avail_md5s
      }, FALSE)

      if(length(is_key) && !any(is_key)){
        stop("There is no single key shared between you and the server.")
      }

      # use any key
      key <- my_keys[[sample(which(is_key), 1)]]

      answer <- encrypt_string(question, key)

      # my key, server key MD5, key used to answer, question itself, and my answer
      answer <- paste(server_time, server_keymd5, as.character(key$pubkey$fingerprint),
                      question, answer)

      # file request again. If still fails, then let handlers deal with it
      res <- f(
        url = request_url,
        config = do.call(httr::add_headers, conf),
        httr::authenticate(get_user(), answer, "basic"),
        httr::timeout(timeout),
        encode = encode,
        body = body
      )
      authtoken(host = host, port = as.integer(port), answer)
    }
  }, error = function(e){
    e$message <- sprintf("%s\n\n  Please make sure you have the access to the server.\n", e$message)
    warning(e)
  })
  res

}

#' @rdname restbatch-requests
#' @export
request_task_list <- function(task_status = 'valid', host = default_host(allow0 = FALSE),
                              port = default_port(), protocol = default_protocol(), path = 'task/list'){
  # url <- sprintf('%s://%s:%d/%s', protocol, host, port, path)
  res <- request_server(path = path, host = host,
                        port = port, protocol = protocol,
                        body = list(status = task_status), method = 'POST')
  content <- httr::content(res)

  content <- do.call('rbind', lapply(content, function(item){
    as.data.frame(item, stringsAsFactors = FALSE)
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

