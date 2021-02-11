

keygen <- function(){
  key <- openssl::rsa_keygen()
  pubkey <- key$pubkey
  # raw bytes to numbers
  list(
    private = as.character(openssl::bignum(key)),
    public = as.character(openssl::bignum(pubkey))
  )
}

#' Give additional user server access
#' @description See examples on how to give additional remote users access.
#' @param userid user's ID
#' @param role role of the user; choices as 'user' (default) and 'admin'
#' @param password a secret code to encode/decode the private key. This is not
#' the user's password. It is just some code that only is shared between the
#' system admin and the target user. If \code{password} is not blank, then
#' even if someone gets your \code{pem_file}, the private key is still safe.
#' @param username the user's name
#' @param pem_file where the encrypted key should be saved to
#' @param dry_run to display the result instead of actually adding the user
#' to the database
#' @param overwrite whether to overwrite the private keys if the user already
#' exists in the database
#' @param force whether to force adding the user. This is usually used with
#' \code{overwrite}. If the user already exists, \code{force=FALSE} will raise
#' errors or prompt for options. If \code{force=TRUE}, then the key will be
#' added. Depending on \code{overwrite} choice, if \code{overwrite} is set to
#' false, then multiple keys will coexist for the same user and they can use
#' any one to file task requests.
#'
#' @examples
#'
#'
#' # ----------- If you are the server admin -----------
#' # The server admin need to get end user's user ID and a user name
#' # To add the user,
#'
#' # assuming the user ID is as follows (I created a fake one)
#' userid <- "22dbdc9e876a74a768c15e03c7356780"
#' username <- "Alice"
#' pem <- generate_pem(userid = userid, username = username, force = TRUE)
#'
#' # Now share the passcode and pen_file to the end user
#' # password is
#' pem$password
#'
#' # encoded pem_file is stored at
#' pem$pem_file
#'
#' # ---------- If you are the end user ----------
#'
#' # Send your user ID and your preferred user name to the system admin
#' # user ID can be obtained from `my_userid()`
#'
#' my_userid()  # something like "22dbdc9e876a74a768c15e03c7356780" (32-length)
#'
#' # You will get a passcode and a pem file from the system admin
#' # (see variable `pem` in the previous example)
#'
#' add_pem(pem$pem_file, pem$password, username = 'Alice')
#'
#' @name restbatch-user
NULL

#' @rdname restbatch-user
#' @export
generate_pem <- function(userid, role = c("user", "admin"), password = rand_string(8), username = "Unknown",
                         pem_file = tempfile(), dry_run = FALSE, overwrite = FALSE, force = FALSE){
  role <- match.arg(role)
  userid <- clean_db_entry(userid, msg = "userid must be combinations of letters and digits with length 32.")
  if(stringr::str_length(userid) != 32){
    stop("userid must be combinations of letters and digits with length 32.")
  }
  username <- clean_db_entry(
    username, disallow = "[^a-zA-Z0-9-_.]",
    msg = "username must be combinations of letters, digits, dash, underscore, and/or space")

  key <- openssl::rsa_keygen()
  private_key <- as.character(openssl::bignum(key))

  if(!dry_run){
    db_adduser(userid = userid, private_key = private_key, username = username,
               overwrite = overwrite, force = force, role = role)
  }
  if(pem_file == stdout()) {
    message("Please save this key to a file")
  }

  openssl::write_pem(key, pem_file, password = password)
  # openssl::read_key(pem_file, 'sad')

  invisible(list(
    userid = userid,
    pem_file = pem_file,
    password = password
  ))
}

#' @rdname restbatch-user
#' @export
my_userid <- function(){
  get_user()
}

#' @rdname restbatch-user
#' @export
add_pem <- function(pem_file, password = '', username = get_username()) {
  multiple_keys <- 'keep_both'
  key <- openssl::read_key(pem_file, password)
  private_key <- as.character(openssl::bignum(key))
  userid <- get_user()
  username <- clean_db_entry(username, '[^a-zA-Z0-9_-]', msg = "invalid user name. Please only include letters, digits, `-` and/or `_`.")

  # check if the key exists
  keys <- private_key(userid)
  if(any(vapply(keys, function(k){ identical(k, key) }, FUN.VALUE = FALSE)) && multiple_keys != "replace"){
    message("This private key already exists.")
  } else {
    if(multiple_keys == 'keep_both'){
      overwrite <- FALSE
      force <- TRUE
    } else if(multiple_keys == 'stop') {
      overwrite <- FALSE
      force <- FALSE
    } else {
      force <- FALSE
      overwrite <- TRUE
    }
    db_adduser(userid = userid, private_key = private_key, username = username, overwrite = overwrite, force = force)
  }
}

# f <- tempfile()
# generate_pem(get_user(), '123', pem_file = f, dry_run = TRUE)$key
# add_pem(f, '123w')

