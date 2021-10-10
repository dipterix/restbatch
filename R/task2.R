STATUS_CODE <- list(
  '0' = "init",
  '1' = 'running',
  '2' = 'finish',
  '-1' = 'canceled'
)


task__local_status <- function(task){
  task$reload_registry(writeable = FALSE)
  batchtools::getStatus(reg = task$reg)
}
task__submit <- function(task, pack = NA, force = FALSE){

  if(task$submitted && !force){
    stop("Task has been submitted. To re-submit to the server, use `force=TRUE`")
  }

  if(is.na(pack)){
    # check whether host is local
    if(host_is_local(task$host, include_public = NA)){
      # server and client share the same task dir, no need to pack
      pack <- FALSE
    } else {
      # Need to pack
      pack <- TRUE
    }
  }

  pack <- isTRUE(pack)
  # conf <- prepare_request()
  #
  # conf$path <- task$task_dir
  # conf$task_name <- task$task_name
  # conf$standalone <- pack
  # conf <- as.list(conf)
  # names(conf) <- sprintf("restbatch.%s", names(conf))

  if(pack) {
    encode <- 'multipart'
    body <- list(datapak = httr::upload_file(task__zip(task), type = 'application/zip'))
  } else {
    encode <- 'json'
    body <- NULL
  }

  res <- request_server(
    host = task$host, port = task$port, protocol = task$protocol, path = task$path_submit,
    header = list(
      path = task$task_dir,
      task_name = task$task_name,
      standalone = pack
    ), encode = encode, body = body
  )
  # res <- httr::POST(
  #   url = sprintf("%s://%s:%d/%s", task$protocol, task$host, task$port, task$path_submit),
  #   config = do.call(httr::add_headers, conf),
  #   encode = encode,
  #   body = body
  # )
  # httr::has_content(res)
  # httr::content(res)
  res

  tryCatch({
    scode <- httr::status_code(res)
  }, error = function(e){
    stop("Unable to reach the server. Job canceled.\nAdditional message: ", e$message)
  })

  try({
    if(scode == 200){

      # Submitted!
      task$submitted <- TRUE

      task$submitted_to$host <- task$host
      task$submitted_to$port <- task$port
      task$submitted_to$path <- task$path_submit
      task$submitted_to$protocol <- task$protocol
      task$submitted_to$packed <- pack
      content <- httr::content(res)

      if(!(is.list(content) && isTRUE(unlist(content$message) == "Job submitted."))){
        warning("Job submitted, but got incorrect response from the server. Please check your server configurations and make sure you trust the the host.")
      }

      # check with server to make sure the task has been submitted (sometimes the server could get errors)
      server_status <- task$server_status()
      if(server_status$status == 'unknown' && is.na(server_status$n_total)){
        # server did not receive the task correctly, raise error
        task$submitted <- FALSE
        stop("Task submitted, but the server is too busy. Please submit again.")
      }

      # update
      db_update_task_client(task)

      return(res)

    } else {

      # scode not 200
      content <- tryCatch({

        content <- httr::content(res)
        content <- paste(unlist(content$error), collapse = "\n")
        content

      }, error = function(e){
        "Unknown error while submitting the task"
      })

      stop("Server error code: ", scode, ". Messages: ", content)

    }
  })


  res
}

task__server_status <- function(task){
  # task$path_status <- 'task/status'
  status <- list(
    status = 'unknown',
    error = NA,
    timestamp = NA,
    message = "Server cannot find the task.",
    n_total = NA,
    n_started = NA,
    n_done = NA,
    n_error = NA
  )

  if(!task$submitted){
    warning("Task has not been submitted to the server. Please run task$submit() first.")
    return(status)
  }
  # url <- sprintf("%s://%s:%d/%s", task$protocol, task$submitted_to$host, task$submitted_to$port, task$path_status)
  res <- request_server(path = task$path_status, host = task$submitted_to$host,
                        port = task$submitted_to$port, protocol = task$protocol,
                        body = list(task_name = task$task_name))

  if(res$status_code == 200){
    content <- httr::content(res)
    if(length(content)){
      if(isTRUE(content$name == task$task_name)){
        if(is.character(content$status)){
          status$status <- content$status
        } else {
          status$status <- STATUS_CODE[[sprintf("%.0f", content$status)]]
        }

        if(length(status$status)){
          status$error <- isTRUE(content$error > 0)
          status$timestamp <- as.POSIXct(content$time_added, origin="1970-01-01")
          status$message <- "OK"
          status$n_total <- as.integer(content$n_total)
          status$n_started <- as.integer(content$n_started)
          status$n_done <- as.integer(content$n_done)
          status$n_error <- as.integer(content$n_error)
        } else {
          status$status <- NA
          status$message <- "Invalid server response"
        }
      }
    }
  } else {
    status$message <- httr::content(res, as = 'text', encoding = 'UTF-8')
  }

  status
}

task__resolved <- function(task){
  if(task$collected){
    return(TRUE)
  }

  # local file shows resolved, no need to download
  if(task$locally_resolved()){
    return(TRUE)
  }

  # check server status
  if(!task$submitted){
    stop("Task has not been submitted to the server. Please run task$submit() first.")
  }

  # check task status
  # db_get_task(userid = get_user(), client = FALSE, status = 'all')

  tryCatch({
    status <- task$server_status()

    # still running
    if(isTRUE(status$status %in% c('running', 'init'))){
      return(FALSE)
    }

    if(isTRUE(status$status %in% "finish")){

      # check again because it could be finished during the queries
      packed <- FALSE
      if(!task$locally_resolved()){
        packed <- TRUE
        task$download()
      }

      if(task$locally_resolved()) {
        return(TRUE)
      } else {
        if(packed){
          # Most likely to be an error because server marked as finished, but wrapped an unfinished task
          # could happen if task is re-submitted, but server then should mark it as init or running
          stop("Task downloaded from the server is unfinished. The task files is at\n", task$task_dir, ".zip\n\nPlease report this issue to `https://github.com/dipterix/restbatch/issues`.")
        }
        return(FALSE)
      }


    }

    if(isTRUE(status$status %in% "canceled")){
      stop("Job has been canceled by the server.")
    }

    if(!length(status$n_total)){
      stop("Server has removed the task.")
    }

  }, error = function(e){

    # Server is not running
    s <- task$local_status()
    if(s$done + s$error >= task$njobs && s$running == 0){
      return(TRUE)
    } else {
      stop("\nFailed to get tasks status from the server.\nServer is shutdown or the task is canceled/removed.\n\nAdditional message: \n", e)
    }

  })

}

task__collect <- function(task){

  if(task$collected){
    return(task$results)
  }
  task$reload_registry(writeable = FALSE)
  await <- 0.5
  while(!task$resolved()){
    Sys.sleep(await)
    await <- await * 1.15
    if(await > 10){ await <- 10 }
  }
  # get job IDs
  tbl <- batchtools::getJobTable(reg = task$reg)
  error_table <- batchtools::getErrorMessages(reg = task$reg)

  ret <- structure(
    lapply(seq_len(nrow(tbl)), function(ii){
      if(!is.na(tbl$error[[ii]]) && is.character(tbl$error[[ii]])){
        return(simpleError(message = tbl$error[[ii]], call = NULL))
      } else {
        job_id <- tbl$job.id[[ii]]
        batchtools::loadResult(job_id, reg = task$reg)
      }
    }),
    has_error = nrow(error_table) > 0,
    error_table = error_table,
    batch_table = tbl,
    task = task,
    class = c("restbatch.result", "list")
  )

  # batchtools::getErrorMessages(reg = task$reg)
  #
  # res <- batchtools::reduceResultsList(reg = task$reg)
  task$results <- ret
  task$collected <- TRUE

  db_update_task_client(task)

  ret
}

task__local_results <- function(task){
  s <- task$locally_resolved()
  if(!s){
    stop("Task not ready to pick up.")
  }
  # collect
  tbl <- batchtools::getJobTable(reg = task$reg)

  ret <- lapply(seq_len(nrow(tbl)), function(ii){
    if(!is.na(tbl$error[[ii]]) && is.character(tbl$error[[ii]])){
      return(simpleError(message = tbl$error[[ii]], call = NULL))
    } else {
      job_id <- tbl$job.id[[ii]]
      batchtools::loadResult(job_id, reg = task$reg)
    }
  })

  ret
}

task__clear_registry <- function(task){
  task$reload_registry(writeable = TRUE)
  batchtools::clearRegistry(task$reg)
}
task__remove <- function(task, wait = 0.01){

  if(dir.exists(task$task_dir)){
    task$reload_registry(writeable = TRUE)
    suppressMessages({
      batchtools::removeRegistry(wait = wait, reg = task$reg)
    })
    unlink(task$task_dir, recursive = TRUE)
  }

  if(!isin_server()){
    try({
      # de-register from the database
      # The task will be marked as "removed"
      db_update_task_client(task)
    }, silent = TRUE)

    try({
        request_server(path = 'task/remove', host = task$submitted_to$host,
                       port = task$submitted_to$port, protocol = task$protocol,
                       body = list(task_name = task$task_name))
      return(TRUE)
    }, silent = TRUE)
  }
  return(FALSE)
}
task__zip <- function(task, target = tempfile(fileext = '.zip'), result_only = FALSE){

  if(isin_server()){
    target <- paste0(task$task_dir, '.zip')
  }

  # zip the directory
  f <- target
  if(file.exists(f)){ unlink(f) }
  wd <- getwd()
  on.exit({
    setwd(wd)
  }, add = TRUE, after = TRUE)
  setwd(task$task_root)

  if(result_only){
    # Usually this means the task has been resolved, and globals are not needed anymore

    if(!isin_server() && interactive()){
      warning("Trying to zip a task without globals. This will almost surely result in error. Should only be used on the server implementation.")
    }

    sub_dir <- list.files(task$task_name)
    sub_dir <- sub_dir[sub_dir != "restbatch"]
    utils::zip(f, file.path(task$task_name, sub_dir))
  } else {
    utils::zip(f, task$task_name)
  }

  normalizePath(f)
}
task__download <- function(task, target, force = FALSE){

  # check if the task is finished locally
  if(!force && task$locally_resolved()){
    return(FALSE)
  }

  # url <- sprintf('%s://%s:%d/%s', task$protocol, task$host, task$port, task$path_download)
  res <- request_server(path = task$path_download, host = task$host,
                        port = task$port, protocol = task$protocol,
                        list(task_name=task$task_name, force=force))
  if(res$status_code == 418){
    err <- httr::content(res)
    stop(simpleError(err$error[[1]], call = NULL))
  } else if(res$status_code == 200 && res$headers$`content-type` == 'application/zip'){
    f <- tempfile(fileext = '.zip')
    writeBin(httr::content(res), f)
    files <- utils::unzip(f, list = TRUE)
    files <- files$Name[startsWith(files$Name, task$task_name)]
    utils::unzip(f, exdir = task$task_root, files = files, overwrite = TRUE)
    unlink(f)
    return(TRUE)
  } else {
    stop("Unknwon status code from the server.")
  }
}
task__reload_registry <- function(task, writeable = FALSE){
  # TODO: check if the task has been submitted, if so, raise error when
  # the session is not the server
  suppressMessages({
    task$reg <- batchtools::loadRegistry(task$task_dir, work.dir = task$task_root,
                                         make.default = FALSE, writeable = writeable)
  })
}

task__locally_resolved <- function(task){
  s <- task$local_status()
  if(s$running == 0 && s$done + s$error >= task$njobs){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


task__monitor_rstudio <- function(task, update_freq = 1, ...){

  n_finished <- 0
  submitted <- task$submitted
  readable_name <- stringr::str_remove_all(task$task_name, "(^[a-zA-Z0-9]{32}__)|(__[a-zA-Z0-9]{16})")

  job <- rstudioapi::jobAdd(
    name = sprintf("%s (batch jobs)", readable_name), status = "Initializing...",
    progressUnits = task$njobs, autoRemove = FALSE,
    show = TRUE, running = FALSE,
    actions = list(
      "stop" = function(id){
        print(id)
        rstudioapi::jobRemove(job)
      }
    )
  )

  rstudioapi::jobAddOutput(job = job, output = sprintf("Task ID name: %s\n", task$task_name))
  rstudioapi::jobAddOutput(job = job, output = sprintf("Task local path: %s\n", task$task_dir))
  if(!submitted){
    rstudioapi::jobAddOutput(job = job, output = sprintf("\nTask has not been submitted/scheduled. Please run the following command to submit task:\n\n"))
    if(server_alive()){
      rstudioapi::jobAddOutput(job = job, output = "\ttask$submit()\n\n")
    } else {
      rstudioapi::jobAddOutput(job = job, output = "\tensure_server()\n\ttask$submit()\n\n")
    }
  }

  callback <- function(){
    complete <- tryCatch({

      complete <- FALSE

      if(!submitted && task$submitted){
        submitted <<- TRUE
        rstudioapi::jobAddOutput(job = job, output = sprintf("Jobs submitted to: %s:%s\n", task$submitted_to$host, task$submitted_to$port))
        rstudioapi::jobAddOutput(job = job, output = sprintf(
          "\nYou can download the task results once it's finished: \n\t%s://%s:%s/info/results?task_name=%s\n",
          task$protocol, task$submitted_to$host, task$submitted_to$port, task$task_name))
        rstudioapi::jobAddOutput(job = job, output = "\nStart listening to the server report...\n")
      } else if(task$submitted) {
        s <- task$server_status()
        msg <- sprintf("Total %s jobs: %d started, %d finished, %d errors\n", task$njobs, s$n_started, s$n_done, s$n_error)

        rstudioapi::jobSetStatus(job = job, status = msg)
        finished <- s$n_done + s$n_error
        if(!isTRUE(n_finished == finished)){
          n_finished <<- finished
          rstudioapi::jobAddOutput(job = job, output = sprintf("%d finished out of %d\n", n_finished, task$njobs))
          rstudioapi::jobSetProgress(job = job, units = n_finished)
        }



        if(s$status == "running"){
          rstudioapi::jobSetState(job = job, state = "running")
        } else if(s$status == "finish"){
          if(s$error){
            rstudioapi::jobSetState(job = job, state = "failed")
            rstudioapi::jobAddOutput(job = job, output = "\nTask finished with errors\n")
          } else {
            rstudioapi::jobSetState(job = job, state = "succeeded")
            rstudioapi::jobAddOutput(job = job, output = "\nTask finished\n")
          }
          complete <- TRUE
        } else if (s$status == "canceled") {
          rstudioapi::jobSetState(job = job, state = "canceled")
          rstudioapi::jobAddOutput(job = job, output = "\nTask canceled by the server\n")
          complete <- TRUE
        }

      }

      complete

    }, error = function(e){
      rstudioapi::jobSetStatus(job = job, status = e$message)
      TRUE
    })

    if(!complete){
      later::later(callback, delay = update_freq)
    }
  }
  callback()
}

task__monitor_progress <- function(task, update_freq = 1, ..., title) {
  readable_name <- stringr::str_remove_all(task$task_name, "(^[a-zA-Z0-9]{32}__)|(__[a-zA-Z0-9]{16})")
  if(missing(title)){
    title <- readable_name
  }
  p <- dipsaus::progress2(..., title = title, max = task$njobs)

  finished <- -1

  callback <- function(){
    complete <- FALSE
    if(finished == -1 && task$submitted){
      finished <<- 0
      p$inc(detail = "Added to task queue", message = "Initialized", amount = 0)
    } else if(task$submitted) {
      complete <- tryCatch({
        s <- task$server_status()
        n_f <- s$n_done + s$n_error
        if(n_f != finished){
          msg <- sprintf("%d (%d errors) of %d finished", n_f, s$n_error, task$njobs)
          p$inc(detail = msg, message = s$status, amount = n_f - finished)
          finished <<- n_f
        }
        complete <- s$status %in% c("finished", "canceled")
        if(complete){
          p$close(s$status)
        }
        complete
      }, error = function(e){
        p$inc(paste("Error while getting status:", paste(e$message, collapse = '')))
        TRUE
      })
    }


    if(complete){
      if(!p$is_closed()){
        p$close()
      }
    } else {
      later::later(callback, delay = update_freq)
    }
  }
  callback()
}

task__monitor_callback <- function(task, update_freq = 1, ..., callback) {

  callback(status = "not submitted", submitted = task$submitted,
           total = task$njobs, finished = NA, error = NA)
  finished <- -1
  loop <- function(){

    complete <- FALSE
    if(finished == -1 && task$submitted){
      finished <<- 0
      callback(status = "init", submitted = task$submitted,
               total = task$njobs, finished = 0, error = 0)
    } else {
      complete <- tryCatch({
        s <- task$server_status()
        n_f <- s$n_done + s$n_error
        if(n_f != finished){
          finished <<- n_f
          callback(status = s$status, submitted = task$submitted,
                   total = task$njobs, finished = finished, error = s$n_error)
        }
        complete <- s$status %in% c("finished", "canceled")
        complete
      }, error = function(e){
        callback(status = "callback error", submitted = task$submitted,
                 total = task$njobs, finished = NA, error = NA)
        TRUE
      })
    }


    if(!complete){
      later::later(loop, delay = update_freq)
    }

  }

  loop()
}

task__monitor <- function(task, mode = c("rstudiojob", "progress", "callback"), callback = NULL, ..., update_freq = 1){
  mode <- match.arg(mode)

  if(mode == 'rstudiojob'){
    if(!dipsaus::package_installed('rstudioapi')){
      message("Package `rstudioapi` not installed; fallback to 'progress'")
      mode <- 'progress'
    } else if(!rstudioapi::isAvailable('1.3.1')) {
      message("Not in RStudio or the version is too low; fall back to 'progress'")
      mode <- 'progress'
    }
  } else if(mode == "callback" && is.null(callback)){
    mode <- "progress"
  }

  switch (
    mode,
    'progress' = {
      task__monitor_progress(task, update_freq = update_freq, ...)
    },
    'callback' = {
      task__monitor_callback(task, update_freq = update_freq, ..., callback = callback)
    },
    'rstudiojob' = {
      task__monitor_rstudio(task, update_freq = update_freq, ...)
    }
  )

}

task__set_globals <- function(task, .env = parent.frame(), ..., .list = list(),
                         .auto_vars = FALSE, .packages = character(0), .dry_run = FALSE){

  if(task$submitted){
    stop("The task has been submitted. Cannot change exported variables.")
  }

  func <- readRDS(file.path(task$exports_dir, "func.rds"))

  globals <- future::getGlobalsAndPackages(dipsaus::new_function2(
    func$fun_args, func$fun_body, env = .env, quote_type = "quote"
  ), envir = .env, maxSize = Inf, globals = .auto_vars)

  packages <- unique(c(.packages, globals$packages))
  globals <- globals$globals
  where <- attr(globals, "where")

  more_globals <- c(list(...), .list)

  for(nm in names(more_globals)){
    globals[[nm]] <- more_globals[[nm]]
    where[[nm]] <- NULL
  }
  attr(globals, "where") <- where
  attr(globals, "packages") <- packages

  attr(globals, "total_size") <- NULL

  nms <- names(globals)
  stopifnot2(!('' %in% nms),
             msg = "Invalid global variable name detected. All variables should be named.")

  if(!.dry_run){
    saveRDS(globals, file.path(task$exports_dir, "globals.rds"))
    saveRDS(nms, file.path(task$exports_dir, "globals.names.rds"))
    return(invisible(globals))
  } else {
    message(".dry_run=TRUE will not save the variables to task. It's designed to debug tasks.")
    return(globals)
  }
}

task__get_globals <- function(task){
  f <- file.path(task$exports_dir, "globals.rds")
  if(file.exists(f)){
    return(readRDS(f))
  } else {
    NULL
  }
}

new_task_internal <- function(task_root, task_dir, task_name, reg){
  if(missing(reg)){
    suppressMessages({
      reg <- batchtools::loadRegistry(task_dir, work.dir = get_task_root(),
                                      make.default = FALSE, writeable = FALSE)
    })
  }
  task <- dipsaus::list_to_fastmap2(list(
    # url path
    protocol = default_protocol(),
    host = default_host(allow0 = FALSE),
    port = default_port(),
    path_validate = "validate/ping",
    path_submit = "task/new",
    path_status = 'task/status',
    path_download = 'task/download',

    # fields
    reg = reg,
    task_name = task_name,
    task_dir = task_dir,
    task_root = task_root,
    exports_dir = file.path(task_dir, "restbatch"),
    njobs = nrow(reg$status),
    results = NULL,
    collected = FALSE,
    submitted = FALSE,
    submitted_to = dipsaus::fastmap2(),

    # used by server only (no use for clients)
    ..server_status = 0L,  # STATUS_CODE
    ..server_packed = FALSE,
    ..server_time_added = as.numeric(Sys.time()),

    # methods
    local_status = function(){ task__local_status(task) },
    server_status = function(){ task__server_status(task) },
    set_globals = function(.env = parent.frame(), ..., .list = list(),
                      .auto_vars = FALSE, .packages = character(0), .dry_run = FALSE){

      if(isin_server()){
        stop("Cannot set task globals within a task function.")
      }

      task__set_globals(task, ..., .env = parent.frame(), .list = .list,
                        .auto_vars = .auto_vars, .dry_run = .dry_run,
                        .packages = .packages)
    },
    get_globals = function(){ task__get_globals(task) },
    submit = function(pack = NA, force = FALSE){ task__submit(task, pack = pack, force = force) },
    resolved = function(){ task__resolved(task) },
    clear_registry = function(){ task__clear_registry(task) },
    remove = function(wait = 0.01){ task__remove(task, wait = wait) },
    reload_registry = function(writeable = FALSE){ task__reload_registry(task, writeable = writeable) },
    collect = function(){ task__collect(task) },
    locally_resolved = function(){ task__locally_resolved(task) },
    zip = function(target = tempfile(fileext = '.zip'), result_only = FALSE){ task__zip(task, target, result_only) },
    download = function(target, force = FALSE){ task__download(task, target, force = FALSE) },
    monitor = function(mode = c("rstudiojob", "progress", "callback"), callback = NULL, update_freq = 1, ...){
      if(isin_server()){
        stop("Cannot monitor a task within a task function.")
      }
      task__monitor(task, mode = mode, callback = callback, update_freq = update_freq, ...)
    },

    # debug
    ..view = function(){
      system(sprintf("open \"%s\"", task$task_dir))
    },
    ..local_results = function(){
      task__local_results(task)
    }
  ))

  class(task) <- c("restbatch.task", "fastmap2", "list")
  task

}

new_task <- function(fun, ..., task_name, .temporary = FALSE){
  if(missing(task_name)){
    task_name <- "noname"
  }
  task_name <- clean_db_entry(
    task_name, "[^A-Za-z0-9-_]",
    msg = sprintf("[2] Invalid task name [%s]. Can only contains letters, digits, and `-`, `_`", task_name))
  task_dir <- get_task_path(task_name)
  task_name <- attr(task_dir, 'task_name')
  task_root <- attr(task_dir, 'task_root')
  exports_dir <- file.path(task_dir, "restbatch")
  suppressMessages({

    reg <- batchtools::makeRegistry(file.dir = task_dir, work.dir = task_root,
                                    namespaces = attached_packages(), make.default = FALSE)

    # mod the function body to load global variables
    fun_body <- body(fun)
    body(fun) <- bquote({
      # Obtain the globals
      local({
        # will run in another process
        f <- file.path(.(task_name), "restbatch", "globals.rds")
        last_loaded <- isTRUE(getOption("restbatch_globals_loaded", "") == f)
        if(file.exists(f) && !last_loaded){
          globals <- readRDS(file.path(.(task_name), "restbatch", "globals.rds"))
          for(pkg in attr(globals, "packages")){
            # load & attach namespace
            # Have to do this as this is running in another process.
            # The user's code might not in ns::func format
            # in this case, the namespace must be loaded, otherwise the function
            # cannot be found
            do.call("require", list(package = pkg, quietly = TRUE, character.only = TRUE))
          }
          list2env( globals, envir = .GlobalEnv )
          options("restbatch_globals_loaded" = f)
        }
      })
      .(fun_body)
    })

    batchtools::batchMap(fun, ..., reg = reg)

    # exports
    dir_create2(exports_dir)
    saveRDS(list(
      fun_args = formals(fun),
      fun_body = fun_body
    ), file = file.path(exports_dir, "func.rds"))
    # suppressMessages({
    #   batchtools::batchExport(, reg = reg)
    # })

  })
  task <- new_task_internal(task_root, task_dir, task_name, reg)

  if(!.temporary){
    db_update_task_client(task)
  }

  task
}

restore_task <- function(task_name, userid, .client = TRUE, .update_db = TRUE){
  # check database
  task_name <- clean_db_entry(
    task_name, "[^A-Za-z0-9-_]",
    msg = sprintf("[3] Invalid task name [%s]. Can only contains letters, digits, and `-`, `_`", task_name))

  if(.client && missing(userid)){
    userid <- get_user()
  }
  entry <- db_get_task(task_name = task_name, client = .client, status = ifelse(.client, 'valid', 'all'), userid = userid)

  if(!nrow(entry)){
    # cannot find task
    return(NULL)
  }
  entry <- entry[1,]

  task_dir <- entry$path[[1]]
  if(!dir.exists(task_dir)){ return(NULL) }

  task_root <- dirname(task_dir)
  task <- tryCatch({
    task <- new_task_internal(task_root, task_dir, task_name)

    if(.update_db){
      if(.client){
        task$host <- entry$serverip
        task$port <- entry$serverport
        if(entry$submitted || entry$collected){
          task$submitted_to$host <- entry$serverip
          task$submitted_to$port <- entry$serverport
          task$submitted <- TRUE
        }
        # if(entry$collected){
        #   task$collect()
        #   task$results <- batchtools::reduceResultsList(reg = task$reg)
        #   task$collected <- TRUE
        # }
      } else {
        # load from server
        task$..server_status <- entry$status
        task$..server_packed <- (entry$packed > 0)
        task$..server_time_added <- as.numeric(entry$time_added)
      }
    }
    task

  }, error = function(e){
    NULL
  })

  task

}

make_client_task_proxy <- function(task){
  ret <- new.env(parent = emptyenv())

  ret$reload_registry <- function(){
    task$reload_registry(writeable = FALSE)
  }

  # active binding
  makeActiveBinding("port", function(v){
    if(!missing(v)){
      v <- as.integer(v)
      stopifnot(length(v) == 1 && isTRUE(v > 0 && v < 65535))
      task$port <- v
    }
    task$port
  }, ret)

  makeActiveBinding("protocol", function(v){
    if(!missing(v)){
      stopifnot(isTRUE(v %in% c("http", "https")))
      task$protocol <- v
    }
    task$protocol
  }, ret)

  makeActiveBinding("submitted_to", function(){
    list(
      url = sprintf("%s://%s:%d/", task$protocol, task$submitted_to$host, task$submitted_to$port),
      host = task$submitted_to$host,
      port = task$submitted_to$port
    )
  }, ret)

  members <- names(task)
  members <- members[!(startsWith(members, '..') | members %in% names(ret))]

  read_onlys <- c('njobs', 'task_dir', 'results', 'task_root', 'reg', 'task_name', 'exports_dir')

  lapply(members, function(nm){
    if(is.function(task[[nm]])){
      ret[[nm]] <- task[[nm]]
    } else if(nm %in% read_onlys){
      makeActiveBinding(nm, function(){
        task[[nm]]
      }, ret)
    } else {
      makeActiveBinding(nm, function(v){
        if(!missing(v)){
          task[[nm]] <- v
        }
        task[[nm]]
      }, ret)
    }
    return(NULL)
  })

  makeActiveBinding("readable_name", function(){
    stringr::str_remove_all(task$task_name, "(^[a-zA-Z0-9]{32}__)|(__[a-zA-Z0-9]{16})")
  }, ret)

  class(ret) <- "restbatch.task.client"


  lockEnvironment(ret)
  ret
}

#' Create/Restore a client-side task proxy
#' @param fun,... function to apply and additional parameters to be
#' passed to \code{\link[batchtools]{batchMap}} function
#' @param task_name a readable name for the task; default is \code{'Noname'}.
#' @param globals variables to export to the computing processes; see details.
#' @param env environment where the \code{'globals'} are defined
#' @return If the task does not exist, the returns \code{NULL}, otherwise
#' returns a locked environment proxy that wraps batch task with the following
#' fields and methods.
#'
#' @details A task proxy manages a folder at \code{task_dir} containing all
#' the information necessary to perform the task. A task is a collection
#' of "jobs" (see its definition in \code{\link[batchtools]{batchMap}}).
#' The evaluation of the jobs is always in different processes with the current
#' R session, sometimes even different computers.
#'
#' Sometimes the function \code{fun} may contains external objects that need
#' to be exported. For example, \code{function(x){ a+x }} has a variable
#' \code{a} that might be defined in somewhere else. It will raise errors
#' if we simply execute the function. Therefore, the task requires exporting
#' the variable \code{a} before executing jobs. This task could be done
#' automatically or manually.
#' If \code{globals} is \code{TRUE}, then all the external variables including
#' the packages needed will be automatically detected and exported.
#' If \code{globals} is a character vector, then only the variables indicated
#' will be correctly exported
#' If \code{globals} is \code{FALSE}, then you need to manually specify them
#' in \code{task$set_globals} method
#'
#' @section Fields:
#' \describe{
#' \item{\code{task_name}}{task ID, a combination of user ID, task readable
#' name, and a 16-character random string}
#' \item{\code{readable_name}}{a friendly name of the task}
#' \item{\code{task_dir}}{task directory where the jobs are stored}
#' \item{\code{task_root}}{root directory where all tasks are stored}
#' \item{\code{reg}}{\code{'batchtools'} registry that reads batch job status}
#' \item{\code{njobs}}{total number of jobs in the task}
#' \item{\code{protocol}, \code{host}, \code{port}, \code{path_*}}{server
#' to be submitted to, used by \code{'submit()'} method}
#' \item{\code{submitted}}{whether the task has been submitted or not}
#' \item{\code{submitted_to}}{if the task has been submitted, then where}
#' \item{\code{results}}{the result of the task, should almost surely use
#' \code{collect()} instead}
#' \item{\code{collected}}{whether the results has been loaded from task
#' directory into the memory}
#' }
#' @section Methods:
#' \describe{
#' \item{\code{submit}}{submit or re-submit a task. If \code{'pack'} is true,
#' then the task files will be archived before sending to the server. This
#' option is required when server runs remotely.}
#' \item{\code{resolved}}{check whether the task has been resolved, a
#' combination of \code{server_status()} and \code{local_status()}. Will
#' raise errors if server if not running}
#' \item{\code{locally_resolved}}{check whether task is resolved locally without
#' query the server. Only used to check local files. It's possible that
#' server has finished but locally not resolved. See \code{download()} to
#' synchronize local files with the finished tasks on the server}
#' \item{\code{server_status}}{get task status from servers}
#' \item{\code{local_status}}{get task status from servers}
#' \item{\code{download}}{download the finished tasks from the server and
#' overwrite the local files. Raises error if the task is not finished remotely.
#' Always check server status before downloading.}
#' \item{\code{collect}}{collect results. It checks the status both locally
#' and remotely; download the task if finished; load up the results. This
#' function will block the session waiting for results.}
#' \item{\code{zip}}{create an archive that stores the task}
#' \item{\code{clear_registry}}{clear the registry, usually used to re-use the
#' task or to re-run the task}
#' \item{\code{reload_registry}}{reload \code{'reg'} registry under read-only
#' or writable modes. If the task has been submitted, please load in
#' read-only mode to avoid file corrects}
#' \item{\code{remove}}{remove the whole task from the hard drive}
#' }
#'
#' @examples
#'
#' task <- new_task2(function(...){
#'   list(...)
#' }, x = 1:3, y = 1:6)
#'
#' # Not submitted
#' task
#'
#' # save task name to same other places
#' task_name <- task$task_name
#'
#' # submit the task
#' if(interactive()){
#'
#'   ensure_server()
#'
#'   task$submit()
#'
#'   task
#'
#'   # once finished
#'   kill_server()
#'
#' }
#'
#' # Remove the task. You can always restore with task_name
#'
#' rm(task)
#'
#' task_restored <- restore_task2(task_name = task_name)
#'
#' if(task_restored$submitted && task_restored$resolved()){
#'   task_restored$collect()
#' }
#'
#' # clean up, remove all task files
#' task_restored$remove()
#'
#' @name restbench-tasks
NULL

#' @rdname restbench-tasks
#' @export
new_task2 <- function(fun, ..., task_name = "Noname", globals = TRUE, env = parent.frame()) {

  if(isin_server()){
    stop("Cannot call `new_task2` within a task function.")
  }

  stopifnot(is.function(fun))
  task <- new_task(fun = fun, ..., task_name = task_name, .temporary = FALSE)
  task <- make_client_task_proxy(task)
  if(!isFALSE(globals)){
    # if logical or characters
    if(is.logical(globals) || is.character(globals)){
      task$set_globals(.env = env, .auto_vars = globals)
    } else if(is.list(globals)) {
      task$set_globals(.env = env, .auto_vars = FALSE, .list = globals)
    }
  }
  task
}

#' @rdname restbench-tasks
#' @export
restore_task2 <- function(task_name){

  if(isin_server()){
    stop("Cannot call `restore_task2` within a task function.")
  }

  task <- restore_task(task_name = task_name, userid = get_user(), .client = TRUE, .update_db = TRUE)
  if(!is.null(task)){
    task <- make_client_task_proxy(task)
  }
}

#' @export
print.restbatch.task.client <- function(x, ...){
  cat("Task (client proxy from the `restbatch` package)\n")
  cat(sprintf("Task name : [%s]\n", x$task_name))
  cat(sprintf("Total jobs: %d\n", x$njobs))
  if(x$submitted){
    cat(sprintf("Submitted to: %s\n", x$submitted_to$url))

    s <- x$local_status()
    print(s)

    if(x$collected){
      cat("\n*The task result has been collected.\n")
      cat("Use `task$collect()` or `task$results` to get the results\n")
    } else if (s$done + s$error >= x$njobs && s$running == 0){
      cat("\n*The task is finished, but the result has not been collected yet.\n")
      cat("Use `task$collect()` to collect the results.\n")
    } else {
      cat("\n*Use `task$resolved()` to check whether the task has finished.\n")
    }
  } else {
    cat("Submitted to: [NA]\n")
    cat("\n* Use `task$submit()` to submit this task\n")
  }

}
