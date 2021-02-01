get_task_root <- function(){
  task_path <- getOption("restbench.task_root", default = restbench_getopt('task_root'))
  if(length(task_path) != 1 || is.na(task_path)){
    task_path <- "~/rave_data/cache_dir/restbench"
  }
  if(!dir.exists(task_path)){
    dir.create(task_path, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  }
  normalizePath(task_path)
}
get_task_path <- function(task_name, asis = FALSE){
  task_name <- clean_db_entry(
    task_name, "[^A-Za-z0-9-_]",
    msg = sprintf("[1] Invalid task name [%s]. Can only contains letters, digits, and `-`, `_`", task_name))
  uid <- get_user()
  if(!startsWith(task_name, paste0(uid, '__'))){
    task_name <- sprintf("%s__%s", uid, task_name)
  }

  task_root <- get_task_root()
  task_dir <- file.path(task_root, task_name)
  if(!asis && dir.exists(task_dir)){
    task_name <- sprintf("%s__%s", task_name, rand_string(16))
    task_dir <- file.path(task_root, task_name)
  }
  attr(task_dir, 'task_name') <- task_name
  attr(task_dir, 'task_root') <- task_root
  task_dir
}



new_task_internal <- function(task_root, task_dir, task_name, reg){
  suppressMessages({

    ..env <- environment()

    if(missing(reg)){
      reg <- batchtools::loadRegistry(task_dir, work.dir = get_task_root(),
                                      make.default = FALSE, writeable = FALSE)
    }

    server_info <- dipsaus::fastmap2()

    ensure_registry <- function(writeable = FALSE){
      suppressMessages({
        ..env$reg <- batchtools::loadRegistry(task_dir, work.dir = get_task_root(),
                                              make.default = FALSE, writeable = writeable)
      })
      task$reg <- ..env$reg
      task$reg
    }
    status <- function(){
      ensure_registry()
      batchtools::getStatus(reg = reg)
    }
    validate <- function(){
      valid <- FALSE

      tryCatch({
        conf <- prepare_request()

        conf$path <- task_dir
        conf$task_name <- task_name
        conf <- as.list(conf)
        names(conf) <- sprintf("restbench.%s", names(conf))

        encode <- 'json'
        body <- NULL

        res <- httr::POST(
          url = sprintf("%s://%s:%d/%s", task$protocol, task$host, task$port, task$path_validate),
          config = do.call(httr::add_headers, conf),
          encode = encode,
          body = body
        )

        uid <- get_user()
        req_token <- res$request$headers['restbench.tokens']
        token <- httr::content(res)[['token']][[1]]
        keys <- private_key(uid)
        for(key in keys){
          valid <- validate_string(uid, token, req_token)
          if(valid){
            break
          }
        }
        attr(valid, "response") <- res
      }, error = function(e){
        attr(valid, "error") <- e
      })


      valid
    }
    submit <- function(pack = FALSE){
      pack <- isTRUE(pack)
      conf <- prepare_request()

      conf$path <- task_dir
      conf$task_name <- task_name
      conf$standalone <- pack
      conf <- as.list(conf)
      names(conf) <- sprintf("restbench.%s", names(conf))

      if(pack) {
        encode <- 'multipart'
        body <- list(datapak = httr::upload_file(zip(), type = 'application/zip'))
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
          task$submited <- TRUE

          server_info$host <- task$host
          server_info$port <- task$port
          server_info$path <- task$path_submit
          server_info$protocol <- task$protocol
          server_info$packed <- pack
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
    resolved <- function(){
      if(task$collected){
        return(TRUE)
      }
      # load jobs
      s <- status()
      if(s$submitted < task$njobs){
        return(FALSE)
      }
      if(s$done + s$error >= task$njobs && s$running == 0){
        return(TRUE)
      }else {
        return(FALSE)
      }
    }
    collect <- function(){
      if(task$collected){
        return(task$results)
      }
      ensure_registry()
      while(!resolved()){
        Sys.sleep(0.5)
      }
      res <- batchtools::reduceResultsList(reg = task$reg)
      task$results <- res
      task$collected <- TRUE

      db_update_task_client(task)

      res
    }
    clear <- function(){
      ensure_registry(TRUE)
      batchtools::clearRegistry(reg)
    }
    remove <- function(wait = 0.01){
      ensure_registry(TRUE)
      suppressMessages({
        batchtools::removeRegistry(wait = wait, reg = task$reg)
      })
    }
    zip <- function(){
      # zip the directory
      f <- normalizePath(tempfile(fileext = '.zip'), mustWork = FALSE)
      if(file.exists(f)){ unlink(f) }
      wd <- getwd()
      on.exit({
        setwd(wd)
      }, add = TRUE, after = TRUE)
      setwd(task_root)
      utils::zip(f, task_name)
      f
    }

    task <- dipsaus::list_to_fastmap2(list(
      # url path
      protocol = "http",
      host = '127.0.0.1',
      port = 7033,
      path_validate = "validate/ping",
      path_submit = "jobs/new",

      # fields
      reg = reg,
      task_name = task_name,
      task_dir = task_dir,
      task_root = task_root,
      njobs = nrow(reg$status),
      results = NULL,
      collected = FALSE,
      submited = FALSE,
      submited_to = server_info,
      server_status = 0,
      server_packed = FALSE,

      # methods
      status = status,
      submit = submit,
      resolved = resolved,
      collect = collect,
      clear = clear,
      remove = remove,
      validate = validate,
      zip = zip,
      reload_registry = ensure_registry,

      # debug
      ..view = function(){
        system(sprintf("open \"%s\"", task_dir))
      }
    ))
    task
  })
}

#' @export
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

#' @export
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
        if(entry$submited){
          task$submited_to$host <- entry$serverip
          task$submited_to$port <- entry$serverport
          entry$submited <- TRUE
        }
        if(entry$collected){
          task$collect()
        }
      } else {
        # load from server
        task$server_status <- entry$status
        task$server_packed <- (entry$packed > 0)
      }
    }
    task

  }, error = function(e){
    NULL
  })

  task

}


#' @export
queue_task <- function(task, userid){
  .globals <- get('.globals')
  .globals$tasks$add(list(task = task, userid = userid))

  # Watch task
  if(.globals$watchers == 0){
    watch_tasks()
  }

}
