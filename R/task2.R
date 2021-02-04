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
  conf <- prepare_request()

  conf$path <- task$task_dir
  conf$task_name <- task$task_name
  conf$standalone <- pack
  conf <- as.list(conf)
  names(conf) <- sprintf("restbatch.%s", names(conf))

  if(pack) {
    encode <- 'multipart'
    body <- list(datapak = httr::upload_file(task__zip(task), type = 'application/zip'))
  } else {
    encode <- 'json'
    body <- NULL
  }

  res <- httr::POST(
    url = sprintf("%s://%s:%d/%s", task$protocol, task$host, task$port, task$path_submit),
    config = do.call(httr::add_headers, conf),
    encode = encode,
    body = body
  )
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
      # TODO: check whether the server returned expected messages (rev auth)

      content <- httr::content(res)
      # Submitted!
      task$submitted <- TRUE

      task$submitted_to$host <- task$host
      task$submitted_to$port <- task$port
      task$submitted_to$path <- task$path_submit
      task$submitted_to$protocol <- task$protocol
      task$submitted_to$packed <- pack
      if(!(is.list(content) && isTRUE(unlist(content$message) == "Job submitted."))){
        warning("Job submitted, but got incorrect response from the server. Please check your server configurations and make sure you trust the the host.")
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
  # task$path_status <- 'jobs/status'
  status <- list(
    status = 'unknown',
    error = NA,
    timestamp = NA,
    message = "Server cannot find the task."
  )

  if(!task$submitted){
    warning("Task has not been submitted to the server. Please run task$submit() first.")
    return(status)
  }
  url <- sprintf("%s://%s:%d/%s", task$protocol, task$submitted_to$host, task$submitted_to$port, task$path_status)
  res <- request_server(url, body = list(task_name = task$task_name))

  if(res$status_code == 200){
    content <- httr::content(res)
    if(length(content)){
      content <- content[[1]]
      if(isTRUE(content$name == task$task_name)){
        status$status <- STATUS_CODE[[sprintf("%.0f", content$status)]]
        if(length(status$status)){
          status$error <- isTRUE(content$error > 0)
          status$timestamp <- as.POSIXct(content$time_added, origin="1970-01-01")
          status$message = "OK"
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

  }, error = function(e){

    # Server is not running
    s <- task$local_status()
    if(s$done + s$error >= task$njobs && s$running == 0){
      return(TRUE)
    } else {
      stop("\nFailed to get tasks status from the server.\nServer is shutdown or the task is canceled.\n\nAdditional message: \n", e)
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
    await <- await * 1.01
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

task__clear_registry <- function(task){
  task$reload_registry(writeable = TRUE)
  batchtools::clearRegistry(task$reg)
}
task__remove <- function(task, wait = 0.01){
  task$reload_registry(writeable = TRUE)
  suppressMessages({
    batchtools::removeRegistry(wait = wait, reg = task$reg)
  })
}
task__zip <- function(task, target = tempfile(fileext = '.zip')){
  # zip the directory
  f <- target
  if(file.exists(f)){ unlink(f) }
  wd <- getwd()
  on.exit({
    setwd(wd)
  }, add = TRUE, after = TRUE)
  setwd(task$task_root)
  utils::zip(f, task$task_name)
  normalizePath(f)
}
task__download <- function(task, target, force = FALSE){

  # check if the task is finished locally
  if(!force && task$locally_resolved()){
    return(FALSE)
  }

  url <- sprintf('%s://%s:%d/%s', task$protocol, task$host, task$port, task$path_download)
  res <- request_server(url, list(task_name=task$task_name, force=force))
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
    path_submit = "jobs/new",
    path_status = 'jobs/status',
    path_download = 'jobs/download',

    # fields
    reg = reg,
    task_name = task_name,
    task_dir = task_dir,
    task_root = task_root,
    njobs = nrow(reg$status),
    results = NULL,
    collected = FALSE,
    submitted = FALSE,
    submitted_to = dipsaus::fastmap2(),

    # used by server only (no use for clients)
    ..server_status = 0L,  # STATUS_CODE
    ..server_packed = FALSE,

    # methods
    local_status = function(){ task__local_status(task) },
    server_status = function(){ task__server_status(task) },
    submit = function(pack = NA, force = FALSE){ task__submit(task, pack = pack, force = force) },
    resolved = function(){ task__resolved(task) },
    clear_registry = function(){ task__clear_registry(task) },
    remove = function(wait = 0.01){ task__remove(task, wait = wait) },
    reload_registry = function(writeable = FALSE){ task__reload_registry(task, writeable = writeable) },
    collect = function(){ task__collect(task) },
    locally_resolved = function(){ task__locally_resolved(task) },
    zip = function(target = tempfile(fileext = '.zip')){ task__zip(task, target) },
    download = function(target){ task__download(task, target, force = FALSE) },

    # debug
    ..view = function(){
      system(sprintf("open \"%s\"", task$task_dir))
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
  suppressMessages({

    reg <- batchtools::makeRegistry(file.dir = task_dir, work.dir = task_root,
                                    namespaces = attached_packages(), make.default = FALSE)
    batchtools::batchMap(fun, ..., reg = reg)

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

  read_onlys <- c('njobs', 'task_dir', 'results', 'task_root', 'reg', 'task_name')

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

#' @export
new_task2 <- function(fun, ..., task_name = "Noname") {
  stopifnot(is.function(fun))
  task <- new_task(fun = fun, ..., task_name = task_name, .temporary = FALSE)
  make_client_task_proxy(task)
}

#' @export
restore_task2 <- function(task_name){
  task <- restore_task(task_name = task_name, userid = get_user(), .client = TRUE, .update_db = TRUE)
  make_client_task_proxy(task)
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
