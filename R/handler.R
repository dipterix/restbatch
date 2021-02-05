request_authinfo <- function(req, key){
  header <- as.list(req$HEADERS)
  header[[sprintf('restbatch.%s', key)]]
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

  task$reload_registry(TRUE)
  reg <- task$reg

  cat("Sending task: ", task$task_name, '\n')

  settings <- getOption("restbatch.settings")

  f <- future::future({
    load_server_settings(settings)

    # Override cluster functions here (inside of future)
    # workers <- getOption('restbatch.max_concurrent_jobs', 1L)
    # reg$cluster.functions <- batchtools::makeClusterFunctionsSocket(workers, 1)

    eval(parse(file = getOption("restbatch.batch_cluster")))

    batchtools::sweepRegistry(reg = reg)
    batchtools::saveRegistry(reg = reg)

    # This step may take time.
    batchtools::submitJobs(reg = reg)
    # batchtools::submitJobs(reg = reg, ids = 2:4)
    batchtools::waitForJobs(reg = reg)

    cat("Sent: ", task$task_name, '\n')
  })

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

  task_name = req_header$restbatch.task_name
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
  task_name = req_header$restbatch.task_name
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
  userid <- request_authinfo(req, 'userid')
  tokens <- request_authinfo(req, 'tokens')

  auth_enabled <- module_require_auth('validate')

  if(auth_enabled){
    keys <- private_key(userid)
    key <- keys[[1]]

    token <- encrypt_string(tokens[[1]], key)
  } else {
    token <- NULL
  }


  return(list(
    token = token,
    auth_enabled = auth_enabled
  ))

}

#' @rdname restbench-handlers
#' @export
handler_validate_auth <- function(req, res) {

  path <- req$PATH_INFO
  # If path starts with __, #, ?, general, or "", then skip authentication
  path <- sub('^/', '', path)

  if(path == "" || any(startsWith(path, c("__", "#", "?", 'general', "openapi")))){
    plumber::forward()
    return()
  }
  print(path)

  # body <- req$postBody
  auth <- as.list(req$HEADERS[startsWith(names(req$HEADERS), "restbatch.")])

  if(!length(body)){
    res$status <- 401 # Unauthorized
    return(list(error="Invalid request header"))
  }

  time <- auth$restbatch.timestamp
  request_age <- compare_timeStamp(time)
  if(!isTRUE(abs(request_age) < getOption("restbatch.request_timeout", Inf))){
    # This request is made long time ago or faked, fail the auth
    res$status <- 401 # Unauthorized
    return(list(error="Your request has invalid timestamp. Please make sure your system time is synchronized to the world time."))
  }

  userid <- auth$restbatch.userid

  tokens <- auth$restbatch.tokens
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


