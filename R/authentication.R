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
  clean_db_entry(uid)
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
private_to_pubkey <- function(private_key){
  key <- openssl::read_key(openssl::bignum(private_key))
  return(as.character(openssl::bignum(key$pubkey)))
}
private_key <- function(userid){
  uid <- get_user()
  re <- list()
  # Running as local service only get inbound request
  if(getOption('restbench.anonymous_request', TRUE) && isTRUE(userid == uid)){
    re[[1]] <- get_fakekey(public = FALSE)
  }
  # Find user key from database
  user_list <- db_getuser(userid, TRUE)

  ks <- lapply(user_list$private_key, function(k){
    openssl::read_key(openssl::bignum(k))
  })
  unique(c(re, ks))
}
public_key <- function(userid){
  my_uname <- get_user()
  re <- list()
  # Running as local service only get inbound request
  if(getOption('restbench.anonymous_request', TRUE) && isTRUE(userid == my_uname)){
    re[[1]] <- get_fakekey(public = TRUE)
  }
  # Find user key from database
  user_list <- db_getuser(userid, TRUE)

  ks <- lapply(user_list$public_key, function(k){
    openssl::read_pubkey(openssl::bignum(k))
  })

  unique(c(re, ks))
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
