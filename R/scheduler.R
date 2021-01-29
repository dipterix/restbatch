get_timeStamp <- function(){
  time <- Sys.time()
  # as.POSIXlt(time, tz = "GMT")
  strftime(time, tz = "GMT", usetz = TRUE)
}

compare_timeStamp <- function(s, now = Sys.time(), fmt = "%Y-%m-%d %H:%M:%S"){
  time <- strptime(s, fmt, tz = "GMT")
  now <- Sys.time()
  diff <- now - time
  as.numeric(diff, units = "secs")
}

get_user <- function(){
  uid <- restbench_getopt("userid", default = NA)
  if(is.na(uid)){
    uid <- dipsaus::session_uuid()
    restbench_setopt(key = "userid", value = uid)
  }
  uid
}
get_username <- function(){
  uname <- restbench_getopt("username", default = NA)
  if(is.na(uname)){
    user <- Sys.getenv('USER')
    rn <- sample(90000, 1)
    uname <- sprintf('%s-%d', user, rn)
    restbench_setopt(key = "username", value = uname)
  }
  uname
}

get_fakekey <- function(public = TRUE){
  if(public){
    f <- system.file('default_pubkey', package = 'restbench')
    return(openssl::read_pubkey(openssl::bignum(readLines(f, n = 1))))
  } else {
    f <- system.file('default_key', package = 'restbench')
    return(openssl::read_key(openssl::bignum(readLines(f, n = 1))))
  }

}
private_key <- function(userid){
  uid <- get_user()
  re <- list()
  # Running as local service only get inbound request
  if(getOption('restbench.anonymous_request', TRUE) && isTRUE(userid == uid)){
    re[[1]] <- get_fakekey(public = FALSE)
  }
  # TODO: Find user private keys

  re
}
public_key <- function(userid){
  my_uname <- get_user()
  re <- list()
  # Running as local service only get inbound request
  if(getOption('restbench.anonymous_request', TRUE) && isTRUE(userid == my_uname)){
    re[[1]] <- get_fakekey(public = TRUE)
  }
  # TODO: Find user private keys

  re
}

random_key <- function(){
  keys <- private_key(userid = get_user())
  if(!length(keys)){
    stop("Authentication enabled, but you haven't generated any keys yet.")
  }
  sample(keys,1)[[1]]
}

encrypt_raw <- function(data, key){
  stopifnot(is.raw(data))
  if(missing(key)){
    key <- random_key()
  }
  sig <- openssl::signature_create(data, openssl::sha256, key = key)
  as.character(openssl::bignum(sig))
}

encrypt_string <- function(data, key){
  if(missing(key)){
    key <- random_key()
  }
  encrypt_raw(charToRaw(data), key)
}


validate_raw <- function(userid, sig, data){
  stopifnot(is.raw(data))
  keys <- public_key(userid)

  sig_raw <- as.raw(openssl::bignum(sig))

  for(key in keys){
    valid <- openssl::signature_verify(data = data, hash = openssl::sha256,
                                       sig = sig_raw, pubkey = key)
    if(valid){
      return(TRUE)
    }
  }
  return(FALSE)
}

validate_string <- function(userid, sig, data){
  data <- charToRaw(data)
  validate_raw(userid, sig, data)
}

#' @export
validate_auth <- function(req, res) {

  path <- req$PATH_INFO
  # If path starts with __, #, ?, general, or "", then skip authentication
  path <- sub('^/', '', path)

  if(path == "" || any(startsWith(path, c("__", "#", "?", 'general', "openapi")))){
    plumber::forward()
    return()
  }
  print(path)

  body <- req$postBody

  if(!length(body) || is.na(body) || !is.character(body)){
    res$status <- 401 # Unauthorized
    browser()
    print(body)
    return(list(error="Invalid request body"))
  }

  body <- jsonlite::parse_json(body, simplifyVector = TRUE)
  time <- body$timeStamp
  request_age <- compare_timeStamp(time)
  if(abs(request_age) > getOption("restbench.request_timeout", Inf)){
    # This request is made long time ago or faked, fail the auth
    res$status <- 401 # Unauthorized
    return(list(error="Your request has invalid timestamp. Please make sure your system time is synchronized to the world time."))
  }
  auth <- body$auth
  userid <- auth$userid

  tokens <- auth$tokens
  # Encode public token
  # msg <- charToRaw(time)
  # key <- pubkey$ssh

  # token <- rsa_encrypt(msg, pubkey$ssh)
  # rsa_encrypt(msg, pubkey)

  # get public keys
  valid <- FALSE
  for(token in tokens){
    valid <- validate_string(userid = userid, sig = token, data = time)
    if(valid){
      break
    }
  }
  if(!valid){
    res$status <- 401 # Unauthorized
    return(list(error="The signature does not match with existing keys. Invalid request"))
  }

  plumber::forward()
}

#' @export
run_job <- function(req){


  f <- function(path, suggested_workers){

    suppressMessages({
      reg <- batchtools::loadRegistry(path, make.default = FALSE, writeable = TRUE)
      reg$max.concurrent.jobs <- restbench_getopt('max_worker', default = 1L)
      reg$max.concurrent.jobs <- min(reg$max.concurrent.jobs, suggested_workers)

      if(isTRUE(reg$cluster.functions$name == 'Interactive')){
        # check whether new session need to be created
        reg$cluster.functions <- batchtools::makeClusterFunctionsInteractive(external = TRUE)
      }
    })
    # Limit the maximum number of concurrent jobs in the registry
    batchtools::sweepRegistry(reg = reg)
    batchtools::saveRegistry(reg = reg)
    batchtools::submitJobs(reg = reg)

  }
  body <- jsonlite::parse_json(req$postBody, simplifyVector = TRUE)
  if(getOption('restbench.debug', FALSE)){
    f(path = body$path, suggested_workers = body$suggested_workers)
  } else {
    callr::r_bg(f, args = list(path = body$path, suggested_workers = body$suggested_workers),
                supervise = TRUE)
  }
  list(message = "Job submitted.")
}


