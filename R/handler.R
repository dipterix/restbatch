request_authinfo <- function(req, key){
  header <- as.list(req$HEADERS)
  header[[sprintf('restbench.%s', key)]]
}

# task_name <- task$task_name; userid <- restbench:::get_user(); packed=F

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
  task$server_status <- 1L
  db_update_task_server2(task = task, userid = userid)

  task$reload_registry(TRUE)
  reg <- task$reg

  # check whether new session need to be created
  # workers <- getOption('restbench.max_concurrent_jobs', 1L)
  reg$cluster.functions <- batchtools::makeClusterFunctionsInteractive(external = FALSE)

  cat("Sending task: ", task$task_name, '\n')

  future::future({

    batchtools::sweepRegistry(reg = reg)
    batchtools::saveRegistry(reg = reg)

    # This step may take time.
    batchtools::submitJobs(reg = reg)
    # batchtools::submitJobs(reg = reg, ids = 2:4)
    batchtools::waitForJobs(reg = reg)

    cat("Sent: ", task$task_name, '\n')
  })

  .globals$running[[task$task_name]] <- list(
    task = task,
    userid = userid
  )


}

#' @export
handler_unpack_task <- function(req){
  # general flags
  debug <- getOption('restbench.debug', FALSE)
  max_worker <- restbench_getopt('max_worker', default = 1L)

  # parse
  req_header <- as.list(req$HEADERS)
  userid <- req_header$restbench.userid
  workers <- as.integer(req_header$restbench.suggested_workers)
  if(!is.integer(workers)){
    workers <- 1L
  } else if(workers > max_worker){
    workers <- max_worker
  }

  task_name = req_header$restbench.task_name
  packed <- (req_header$restbench.standalone == TRUE)

  # unpack data
  root <- get_task_root()
  if(!dir.exists(root)){ dir_create2(root) }

  # packed data save to disk
  if(packed){

    f <- tempfile(fileext = ".zip")
    writeBin(req$body$datapak$value, f)

    task_dir <- get_task_path(task_name, asis = TRUE)
    if(dir.exists(task_dir)){
      # This is bad, just ignore the datapak because a task with the same name exists
      # If debug, replace the files
      if(debug){
        message("Task path exists, overwrite in debug mode...")
        unlink(task_dir, recursive = TRUE)
        utils::unzip(normalizePath(f), exdir = get_task_root())
      }
    }

    # multipart <- mime::parse_multipart(req)
    # assign('multipart', multipart, envir = globalenv())
    packed <- TRUE
  } else {
    packed <- FALSE
  }

  # restore task
  task_name = req_header$restbench.task_name
  path <- get_task_path(task_name, asis = TRUE)
  suppressMessages({
    reg <- batchtools::loadRegistry(path, work.dir = root,
                                    make.default = FALSE, writeable = TRUE)
    reg$max.concurrent.jobs <- workers
    batchtools::saveRegistry(reg = reg)
  })
  task <- new_task_internal(root, path, task_name, reg)
  task$server_packed <- packed
  task$status()

  # register to the database
  db_update_task_server(task, req)

  task

}

#' @export
handler_query_task <- function(userid, status = 'valid'){
  tasks <- db_get_task(userid = userid, client = FALSE, status = status)
  tasks$path <- NULL
  tasks$clientip <- NULL
  tasks$ncpu <- NULL
  tasks
}

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
  auth <- as.list(req$HEADERS[startsWith(names(req$HEADERS), "restbench.")])

  if(!length(body)){
    res$status <- 401 # Unauthorized
    browser()
    return(list(error="Invalid request header"))
  }

  time <- auth$restbench.timestamp
  request_age <- compare_timeStamp(time)
  if(!isTRUE(abs(request_age) < getOption("restbench.request_timeout", Inf))){
    # This request is made long time ago or faked, fail the auth
    res$status <- 401 # Unauthorized
    return(list(error="Your request has invalid timestamp. Please make sure your system time is synchronized to the world time."))
  }

  userid <- auth$restbench.userid

  tokens <- auth$restbench.tokens
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


