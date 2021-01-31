#' @export
run_jobs <- function(task_name, userid, packed = FALSE){

  # This function will run in globalenv (callr will change its environment, hence need to load namespace)

  ns <- asNamespace('restbench')
  task <- ns$restore_task(task_name = task_name, userid = userid, .client = FALSE, .update_db = FALSE)
  task$server_packed <- as.logical(packed)

  # run!
  task$server_status <- 1L
  ns$db_update_task_server2(task = task, userid = userid)

  reg <- task$reload_registry(TRUE)
  if(isTRUE(reg$cluster.functions$name == 'Interactive')){
    # check whether new session need to be created
    reg$cluster.functions <- batchtools::makeClusterFunctionsInteractive(external = TRUE)
  }
  # Limit the maximum number of concurrent jobs in the registry
  batchtools::sweepRegistry(reg = reg)
  batchtools::saveRegistry(reg = reg)
  batchtools::submitJobs(reg = reg)

  Sys.sleep(0.3)
  while(!task$resolved()){
    Sys.sleep(1)
  }

  # task is resolved, running=0, collected=1, ready for client to get results
  task$server_status <- 2L
  ns$db_update_task_server2(task = task, userid = userid)

}

#' @export
handler_parse_task <- function(req){
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
        unzip(normalizePath(f), exdir = get_task_root())
      }
    }

    # multipart <- mime::parse_multipart(req)
    # assign('multipart', multipart, envir = globalenv())
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
  task$status()

  # register to the database
  db_update_task_server(task, req)

  run_f <- getOption('restbench.func_newjob', run_jobs)
  if(!is.function(run_f)){
    run_f <- eval(parse(text=run_f))
  }

  if(debug){
    run_jobs(task_name, userid = userid, packed = packed)
  } else {
    callr::r_bg(
      run_jobs,
      args = list(
        task_name = task_name,
        userid = userid,
        packed = packed
      ),
      supervise = TRUE,
      stdout = file.path(path, 'restbench.out.log'),
      stderr = file.path(path, 'restbench.err.log')
    )
  }
  list(message = "Job submitted.")
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
  req_header <- as.list(req$HEADERS)
  userid <- req_header$restbench.userid
  tokens <- req_header$restbench.tokens

  keys <- private_key(userid)
  key <- keys[[1]]

  token <- encrypt_string(tokens[[1]], key)

  return(list(
    token = token
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


