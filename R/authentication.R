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
  uid <- restbatch_getopt("userid", default = NA)
  if(is.na(uid)){
    uid <- dipsaus::session_uuid()
    restbatch_setopt(key = "userid", value = uid)
  }
  clean_db_entry(uid)
}
get_username <- function(){
  uname <- restbatch_getopt("username", default = NA)
  if(is.na(uname)){
    user <- Sys.getenv('USER')
    user <- stringr::str_remove_all(user, '[^a-zA-Z0-9_]')
    rn <- sample(90000, 1)
    uname <- sprintf('%s-%d', user, rn)
    restbatch_setopt(key = "username", value = uname)
  } else {
    uname <- stringr::str_remove_all(uname, '[^a-zA-Z0-9_-]')
  }
  uname
}

get_fakekey <- function(public = TRUE){
  if(public){
    f <- system.file('default_pubkey', package = 'restbatch')
    return(openssl::read_pubkey(openssl::bignum(readLines(f, n = 1))))
  } else {
    f <- system.file('default_key', package = 'restbatch')
    return(openssl::read_key(openssl::bignum(readLines(f, n = 1))))
  }

}
private_to_pubkey <- function(private_key){
  key <- openssl::read_key(openssl::bignum(private_key))
  return(as.character(openssl::bignum(key$pubkey)))
}
private_key <- function(userid){
  uid <- get_user()
  re <- list(get_fakekey(public = FALSE))

  # Find user key from database
  user_list <- db_getuser(userid, TRUE)

  ks <- lapply(user_list$private_key, function(k){
    openssl::read_key(openssl::bignum(k))
  })

  # Running as local service only get inbound request
  if(getOption('restbatch.anonymous_request', TRUE) && isTRUE(userid == uid)){
    # use fake key
    unique(c(re, ks))
  } else {
    # remove fake key
    ks[vapply(ks, function(k){ !identical(k, re[[1]]) }, FUN.VALUE = FALSE)]
  }
}
public_key <- function(userid){
  my_id <- get_user()
  re <- list(get_fakekey(public = TRUE))
  # Find user key from database
  user_list <- db_getuser(userid, TRUE)

  ks <- lapply(user_list$public_key, function(k){
    openssl::read_pubkey(openssl::bignum(k))
  })

  # Running as local service only get inbound request
  if(getOption('restbatch.anonymous_request', TRUE) && isTRUE(userid == my_id)){
    unique(c(re, ks))
  } else {
    # remove fake key
    ks[vapply(ks, function(k){ !identical(k, re[[1]]) }, FUN.VALUE = FALSE)]
  }
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


