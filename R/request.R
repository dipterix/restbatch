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

#' @export
request_server <- function(
  url, body = list(), header = list(), method = c('POST', 'GET'), encode = 'json'){
  method <- match.arg(method)
  conf <- prepare_request()
  dipsaus::list_to_fastmap2(header, conf)
  conf <- as.list(conf)
  names(conf) <- sprintf("restbench.%s", names(conf))

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
    e$message <- sprintf("%s\n  Please make sure the server is on.\n", e$message)
    stop(e)
  })
  res

}

#' @export
request_task_list <- function(task_status = 'valid', host = default_host(),
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

# Sweep, and collect tasks
