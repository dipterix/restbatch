request_authinfo <- function(req){
  # header <- as.list(req$HEADERS)
  # header[[sprintf('restbatch.%s', key)]]

  ret <- list()

  authorization <- as.list(req$HEADERS)$authorization
  authorization <- stringr::str_remove(authorization, "^Basic[ ]*")
  authorization <- rawToChar(base64enc::base64decode(authorization))
  authorization <- stringr::str_split_fixed(authorization, ":", 2)

  userid <- authorization[[1]]
  ret$userid <- userid
  authorization <- authorization[[2]]

  # user_key <- private_key(userid)
  #
  # md5s <- sapply(user_key, function(key){ as.character(key$pubkey$fingerprint) })

  request_time <- stringr::str_sub(authorization, end = 23)
  server_md5 <- stringr::str_sub(authorization, start = 25, end = 56)
  client_md5 <- stringr::str_sub(authorization, start = 58, end = 89)
  authorization <- stringr::str_sub(authorization, start = 91)
  authorization <- stringr::str_split_fixed(authorization, " ", 2)
  server_question <- authorization[[1]]
  client_answer <- authorization[[2]]

  ret$request_time <- request_time
  ret$server_md5 <- server_md5
  ret$client_md5 <- client_md5
  ret$server_question <- server_question
  ret$client_answer <- client_answer
  ret
}

# task_name <- task$task_name; userid <- restbatch:::get_user(); packed=F

#' Default handlers to process incoming requests
#' @description \describe{
#' \item{\code{run_task}}{schedules batch jobs and put them to a running list}
#' \item{\code{handler_unpack_task}}{unpack a request and convert to a task}
#' \item{\code{handler_query_task}}{obtain task status on the server}
#' \item{\code{handler_validate_server}}{handles validation request}
#' \item{\code{handler_validate_auth}}{a layer that handles authentication}
#' }
#' @param req,res web request/response variables (see \code{'plumber'} package)
#' @param task a task instance
#' @param userid which user submits the task
#' @param status task status to filter/query; choices are \code{'valid'},
#' \code{'init'} (submitted, waiting to run), \code{'running'} (running
#' task), \code{'finish'} (finished task), and \code{'canceled'} (canceled
#' by the server)
#' @return \code{run_task} returns a named list with:
#' \describe{
#' \item{\code{task}}{task object created by internal \code{new_task} function}
#' \item{\code{userid}}{a 32-character string of user's ID}
#' \item{\code{future}}{a future object that schedules jobs asynchronously}.
#' }
#' \code{handler_unpack_task}, \code{handler_query_task} return task instances
#' created by the internal \code{new_task} function. Others return lists or
#' \code{res} that will be processed by the \code{'plumber'} package.
#'
#' @name restbench-handlers
NULL

#' @rdname restbench-handlers
#' @export
run_task <- function(task, userid){

  # This function will run in globalenv (callr will change its environment, hence need to load namespace)

  # cat("Restoring task: ", task_name, '\n')
  # task <- restore_task(task_name = task_name, userid = userid, .client = FALSE)

  if(is.null(task)){
    stop("Task not found on disk.")
  }

  # run!
  cat("Scheduling task: ", task$task_name, '\n')
  task$..server_status <- 1L
  db_update_task_server2(task = task, userid = userid)

  cat("Sending task: ", task$task_name, '\n')

  settings <- getOption("restbatch.settings")

  task_dir <- task$task_dir
  task_root <- task$task_root

  f <- future::future({
    ..ns <- asNamespace("restbatch")
    ..ns$load_server_settings(settings)

    # Override cluster functions here (inside of future)
    # workers <- getOption('restbatch.max_concurrent_jobs', 1L)
    # reg$cluster.functions <- batchtools::makeClusterFunctionsSocket(workers, 1)

    force(task_dir)
    force(task_root)

    eval(parse(file = getOption("restbatch.batch_cluster")))

    cat("Sent: ", task$task_name, '\n')
  }, packages = c("restbatch"), seed = TRUE)

  list(
    task = task,
    userid = userid,
    future = f
  )


}

#' @rdname restbench-handlers
#' @export
handler_unpack_task <- function(req){
  # general flags
  debug <- getOption('restbatch.debug', FALSE)
  max_worker <- restbatch_getopt('max_worker', default = 1L)

  # parse
  req_header <- as.list(req$HEADERS)
  userid <- req_header$restbatch.userid

  workers <- as.integer(getOption('restbatch.max_concurrent_jobs'))
  if(!length(workers) || is.na(workers[[1]])){
    workers <- NULL
  } else {
    workers <- workers[[1]]
  }

  task_name <- req_header$restbatch.task_name
  packed <- (req_header$restbatch.standalone == TRUE)

  # unpack data
  root <- get_task_root()
  if(!dir.exists(root)){ dir_create2(root) }

  # packed data save to disk
  if(packed){

    f <- tempfile(fileext = ".zip")
    writeBin(req$body$datapak$value, f)

    task_dir <- get_task_path(task_name, asis = TRUE, userid = userid)
    task_root <- get_task_root()
    files <- utils::unzip(f, list = TRUE)
    files <- files$Name[startsWith(files$Name, userid)]
    if(dir.exists(task_dir) && length(files)){
      message(sprintf("Task path for [%s] exists, overwrite...", task_name))
      unlink(task_dir, recursive = TRUE, force = TRUE)
    }
    utils::unzip(f, files = files, overwrite = TRUE, exdir = task_root)

    # multipart <- mime::parse_multipart(req)
    # assign('multipart', multipart, envir = globalenv())
    packed <- TRUE
  } else {
    packed <- FALSE
  }

  # restore task
  task_name <- req_header$restbatch.task_name
  path <- get_task_path(task_name, asis = TRUE, userid = userid)
  suppressMessages({
    reg <- batchtools::loadRegistry(path, work.dir = root,
                                    make.default = FALSE, writeable = TRUE)
    if(!is.null(workers)){
      reg$max.concurrent.jobs <- workers
    }
    batchtools::saveRegistry(reg = reg)
  })
  task <- new_task_internal(root, path, task_name, reg)
  task$..server_packed <- packed
  # task$local_status()

  # register to the database
  db_update_task_server(task, req)

  task

}

#' @rdname restbench-handlers
#' @export
handler_query_task <- function(userid, status = 'valid'){
  tasks <- db_get_task(userid = userid, client = FALSE, status = status)
  tasks$path <- NULL
  tasks$clientip <- NULL
  tasks$ncpu <- NULL
  tasks
}

#' @rdname restbench-handlers
#' @export
handler_validate_server <- function(req){

  auth_enabled <- module_require_auth('validate')

  if(auth_enabled){

    auth_info <- request_authinfo(req)

    # get client key
    keys <- private_key(auth_info$userid)
    keys <- keys[vapply(keys, function(key){ as.character(key$pubkey$fingerprint) %in% auth_info$client_md5 }, FALSE)]
    keys <- keys[[1]]

    server_answer <- encrypt_string(auth_info$client_answer, key = keys)
    auth_info$server_answer <- server_answer

    token <- auth_info
  } else {
    token <- NULL
  }

  return(list(
    token = token,
    auth_enabled = auth_enabled
  ))

}

authenticate <- function(req, res){
  authorization <- as.list(req$HEADERS)$authorization
  authorization <- stringr::str_remove(authorization, "^Basic[ ]*")
  authorization <- rawToChar(base64enc::base64decode(authorization))
  authorization <- stringr::str_split_fixed(authorization, ":", 2)

  userid <- authorization[[1]]
  authorization <- authorization[[2]]

  if(stringr::str_length(userid) != 32){ stop("Invalid user ID") }

  user_key <- private_key(userid)

  if(!length(user_key)){ stop("No such user") }


  md5s <- sapply(user_key, function(key){ as.character(key$pubkey$fingerprint) })

  tryCatch({

    request_time <- stringr::str_sub(authorization, end = 23)

    request_age <- compare_timeStamp(request_time)
    if(!isTRUE(abs(request_age) < getOption("restbatch.keep_alive", Inf))){
      # This request is made long time ago, fail the auth
      stop("Request expired. Please re-auth.")
    }

    server_md5 <- stringr::str_sub(authorization, start = 25, end = 56)
    my_key <- private_key(get_user())
    is_valid <- vapply(my_key, function(key){ as.character(key$pubkey$fingerprint) == server_md5 }, FALSE)

    if(!isTRUE(any(is_valid))){
      stop("Request header has been modified. Auth fails.")
    }

    client_md5 <- stringr::str_sub(authorization, start = 58, end = 89)

    authorization <- stringr::str_sub(authorization, start = 91)
    authorization <- stringr::str_split_fixed(authorization, " ", 2)
    server_question <- authorization[[1]]
    client_answer <- authorization[[2]]

    # make sure the problem is correct
    my_key <- my_key[is_valid][[1]]

    # log information

    valid <- openssl::signature_verify(
      data = charToRaw(request_time),
      hash = openssl::sha256,
      sig = as.raw(openssl::bignum(server_question)),
      pubkey = my_key
    )

    if(!valid){
      stop("Time stamp does not matches with the question token.")
    }

    # makesure the answer is correct
    user_key <- user_key[md5s %in% client_md5]
    if(!length(user_key)){
      stop("The auth token was using an unknown token. Please use the provided options.")
    }

    user_key <- user_key[[1]]


    valid <- openssl::signature_verify(
      data = charToRaw(server_question),
      hash = openssl::sha256,
      sig = as.raw(openssl::bignum(client_answer)),
      pubkey = user_key
    )

    if(!valid){
      stop("The auth failed: wrong answer.")
    }

    cat("[", strftime(Sys.time(), usetz = TRUE), "][auth] userid:", userid, " token_time:", request_time, "\n", sep = "")

  }, error = function(e){

    # current timestamp
    time <- get_timeStamp()

    # The end user may fake the time, so the question is to make sure the
    # time is not faked. Since the question is generated using the owner's
    # key, so if you don't have the owner's key, it's hard (impossible) to
    # generate the question
    my_key <- private_key(get_user())
    my_key <- my_key[[length(my_key)]] # avoid using fake key; TODO remove fake keys

    # use the owner's private key to encode
    question <- encrypt_string(time, my_key)

    # time, md5 of owner's key,
    # keys that can be used to decode the question, question itself
    question <- paste(time, as.character(my_key$pubkey$fingerprint), paste(md5s, collapse = ";"), question)

    # print(res)
    # assign('res', res, envir = globalenv())

    # return the problem to the user
    res$status <- 401

    # return to res
    list(
      userid = userid,
      question = question,
      message = "Authentication required. Please solve this problem.",
      reason = paste(e$message, collapse = '')
    )
  })
}

#' @rdname restbench-handlers
#' @export
handler_validate_auth <- function(req, res) {

  path <- req$PATH_INFO
  # If path starts with __, #, ?, general, or "", then skip authentication
  path <- sub('^/', '', path)

  # assign('req', req, envir = globalenv())
  # print(path)

  ret <- tryCatch({
    authenticate(req, res)
  }, error = function(e){
    res$status <- 403
    list(message = paste(e$message, collapse = ''))
  })

  if(res$status %in% c(500, 403, 401)){
    return(ret)
  }

  plumber::forward()
#
#   # body <- req$postBody
#   auth <- as.list(req$HEADERS[startsWith(names(req$HEADERS), "restbatch.")])
#
#   if(!length(body)){
#     res$status <- 401 # Unauthorized
#     return(list(error="Invalid request header"))
#   }
#
#   time <- auth$restbatch.timestamp
#   request_age <- compare_timeStamp(time)
#   if(!isTRUE(abs(request_age) < getOption("restbatch.keep_alive", Inf))){
#     # This request is made long time ago or faked, fail the auth
#     res$status <- 401 # Unauthorized
#     return(list(error="Your request has invalid timestamp. Please make sure your system time is synchronized to the world time."))
#   }
#
#   userid <- auth$restbatch.userid
#
#   tokens <- auth$restbatch.tokens
#   # Encode public token
#   # msg <- charToRaw(time)
#   # key <- pubkey$ssh
#
#   # token <- rsa_encrypt(msg, pubkey$ssh)
#   # rsa_encrypt(msg, pubkey)
#
#   # get public keys
#   valid <- FALSE
#   for(token in tokens){
#     valid <- validate_string(userid = userid, sig = token, data = time)
#     if(valid){
#       break
#     }
#   }
#   if(!valid){
#     res$status <- 401 # Unauthorized
#     return(list(error="The signature does not match with existing keys. Invalid request"))
#   }

  # plumber::forward()
}


