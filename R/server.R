# TODO: inspect try catch


watch_tasks <- function(){

  debug <- getOption('restbench.debug', FALSE)
  release_speed <- as.numeric(getOption('restbench.max_release_tasks_per_second', 10.0))

  if(.globals$paused){
    if(debug){
      cat("Watch dog is sleeping.\n")
    }
    return(invisible())
  }

  task_names <- names(.globals$running)

  for(nm in task_names){
    item <- .globals$running[[nm]]

    if(is.list(item) && !is.null(item$task)){
      # check
      # 1. submitted, no error
      # 2. resolved?

      remove_task <- FALSE
      ..server_status <- 1L

      if(future::resolved(item$future)){
        tryCatch({
          future::value(item$future)
        }, error = function(e){
          cat("Error while submitting task:", item$task$task_name,'\n')
          cat("Error message: ", e$message,'\n')
          cat("Removing the task from the queue, set status as 'init'...",'\n')
          remove_task <<- TRUE
          ..server_status <<- -1L
        })
      }
      if(!remove_task){
        try({
          # if the task is finished
          if(item$task$locally_resolved()){
            # task is resolved, ready for client to get results
            if(isTRUE(item$task$..server_packed)){
              # create a zip file in the background

              cat("Task ", item$task$task_name, " finished, packing result folder.\n")
              item$packing <- TRUE
              item$future <- future::future({
                item$task$zip(target = paste0(item$task$task_dir, '.zip'))
              })
              remove_task <- FALSE
            } else {
              cat("Task ", item$task$task_name, " finished, updating server database\n")
              remove_task <- TRUE
              ..server_status <- 2L
            }
          }
        })
      }

      if(remove_task){
        tryCatch({
          item$task$..server_status <- ..server_status
          db_update_task_server2(task = item$task, userid = item$userid)
          .subset2(.globals$running, 'remove')(nm)
        }, error = function(e){
          message("Error while updating the database. Force dropping the task... (", e$message,')\n')
          .subset2(.globals$running, 'remove')(nm)
        })
        Sys.sleep(0.1)
        release_speed <- release_speed - 1
        if(release_speed <= 0){
          break
        }
      }

    }

  }


  if(.globals$tasks$size() > 0){

    smry <- server_summary(include_expired = FALSE)
    running_tasks <- smry[['running']]
    max_tasks <- getOption("restbench.max_concurrent_tasks", 1L)

    if(max_tasks > running_tasks){

      cat("Available slot(s):", max_tasks - running_tasks, ".\n")

      # run the first max_tasks-running_tasks tasks
      tasks <- .globals$tasks$mremove(max_tasks - running_tasks)

      # This is a bad name, will change it later to handler_submittask
      run_task <- getOption('restbench.func_newjob', "restbench::run_task")
      if(!is.function(run_task)){
        run_task <- eval(parse(text=run_task))
      }

      lapply(tasks, function(item){
        if(is.null(item)){ return() }
        cat("Starting task:", item$task$task_name, '\n')

        run_task(item$task, userid = item$userid)

      })
    }

  }


  watch_later()

}

watch_later <- function(){

  if(.globals$paused){
    return()
  }

  if(.globals$watchers == 0){

    interval <- getOption('restbench.task_queue_interval', 1)
    interval <- max(interval, 0.1)
    interval <- min(interval, 10)

    later::later(function(){
      # cat("Watching tasks\n")
      .globals$watchers <- 0

      watch_later()

      watch_tasks()

    }, delay = interval)

    .globals$watchers <- .globals$watchers + 1
  }
}



#' @export
conf_sample <- function(file = stdout()){
  s <- readLines(system.file('default_settings.yaml', package = 'restbench'))
  writeLines(s, file)
  # yaml::write_yaml(list(
  #   modules = list(
  #     jobs = "{system.file(\"scheduler/jobs.R\", package = \"restbench\")}",
  #     validate = "{system.file(\"scheduler/validate.R\", package = \"restbench\")}"
  #   ),
  #   options = list(
  #     debug = FALSE,
  #     require_auth = TRUE,
  #     modules_require_auth = "jobs, validate",
  #     request_timeout = Inf,
  #     task_root = "{restbench::restbench_getopt(\"task_root\")}",
  #     max_nodetime = 864000L,
  #     func_newjob = "restbench:::run_task",
  #     func_validate_server = "restbench::handler_validate_server"
  #   )), file)
}

# only use it in the server
module_require_auth <- function(module, settings){
  module %in% getOption('restbench.modules_require_auth_list')
}


load_server_settings <- function(settings){
  if(!is.list(settings)){
    settings <- yaml::read_yaml(settings)
  }
  modules <- settings$modules
  opts <- settings$options
  server_scripts <- settings$server_scripts

  for(nm in names(opts)){

    val <- opts[[nm]]
    if(is.character(val)){
      val <- glue::glue(val)
    }

    do.call("options", structure(list(val), names = sprintf('restbench.%s', nm)))
  }
  # Settings to set options on modules need authentication
  modules_require_auth <- unlist(
    stringr::str_split(getOption("restbench.modules_require_auth",
                                 paste(names(modules), collapse = ',')), "[, ]+"))
  require_auth <- getOption("restbench.require_auth", TRUE)
  if(!require_auth){
    modules_require_auth <- NULL
  }
  options('restbench.modules_require_auth_list' = modules_require_auth)
  options("restbench.settings" = settings)

  # scripts related to server configurations
  startup_script <- glue::glue(server_scripts$startup_script)
  options("restbench.startup_script" = startup_script[file.exists(startup_script)])

  batch_cluster <- glue::glue(server_scripts$batch_cluster)
  options("restbench.batch_cluster" = batch_cluster[file.exists(batch_cluster)])


}

start_server_internal <- function(
  host = default_host(),
  port = default_port(),
  settings = system.file("debug_settings.yaml", package = 'restbench')
){

  default_host(host)
  default_port(port)

  settings <- normalizePath(settings, mustWork = TRUE)
  load_server_settings(settings)
  .globals$paused <- FALSE

  on.exit({
    .globals$paused <- TRUE
  }, add = TRUE, after = TRUE)


  tryCatch({
    eval(parse(file = getOption("restbench.startup_script", NULL)))
  }, error = function(e){
    warning("[settings -> startup_script] is invalid script. Use default setup script")
    future::plan(future::multisession, workers = getOption('restbench.max_concurrent_tasks', 1L) + 1)
  })

  on.exit({
    cat("Cleaning up...\n")

    cat("Shutdown pool\n")
    future::plan(future::sequential)

    # Update the database
    cat("De-register unfinished tasks\n")

    ready_tasks <- .subset2(.globals$tasks, 'as_list')()
    running_tasks <- .subset2(.globals$running, 'as_list')()

    dereg_tasks <- c(ready_tasks, running_tasks)

    if(length(dereg_tasks)){
      # There are running jobs, stop them (set their status to "canceled")
      conn <- db_ensure(close = FALSE)

      for(item in dereg_tasks){
        if(is.list(item) && length(item$userid) ==1 && is.character(item$userid)){
          # No need to clean as it was cleaned when adding to the list
          # userid <- clean_db_entry(item$userid)

          tryCatch({
            DBI::dbExecute(conn, sprintf(
              'UPDATE restbenchtasksserver SET status="-1" WHERE userid="%s" AND name="%s";',
              item$userid, item$task$task_name
            ))
          }, error = function(e){})

        }
      }

      DBI::dbDisconnect(conn)

    }



    cat("Done\n\n\n")
  })

  plumber::pr_run(local({

    settings <- yaml::read_yaml(settings)
    modules <- settings$modules
    modules_require_auth <- getOption('restbench.modules_require_auth_list')

    # Construct Router
    root = plumber::pr()
    current <- root

    # modules require validations

    for(nm in names(modules)){
      mod <- plumber::pr(glue::glue(modules[[nm]]))
      if(nm %in% modules_require_auth){
        mod$filter(name = "validate_auth", expr = handler_validate_auth)
      }
      current <- plumber::pr_mount(current, sprintf("/%s", nm), mod)
    }
    # current$filter(name = "logger", expr = logger)
    # validate_auth

    options("restbench.active_server" = current)

    current$setDebug(debug = getOption("restbench.debug", TRUE))

    current

  }), host = host, port = port)
}

portAvailable <- function(port){
  tryCatch({
    srv <- httpuv::startServer(
      host = default_host(),
      port, list(), quiet = TRUE)
  }, error = function(e) {
    port <<- 0
  }
  )
  if (port != 0) {
    httpuv::stopServer(srv)
  }
  port != 0
}

findPort <- function (port, mustWork = NA) {
  if (missing(port) || is.null(port)) {
    port <- as.integer(restbench_getopt("default_port", default = 7033))
    if(length(port) != 1 || is.na(port) || !is.integer(port)){
      port <- httpuv::randomPort()
    }
    for (i in 1:11) {
      if(!portAvailable(port)){
        port <- httpuv::randomPort()
      } else {
        restbench_setopt('default_port', port, .save = FALSE)
        break
      }
    }
  }
  if (port == 0) {
    msg <- "Unable to start a server. Either the port specified was unavailable or we were unable to find a free port."
    if(isTRUE(mustWork)){
      stop(msg)
    } else if(is.na(mustWork)){
      warning(msg)
    }
  }
  port
}


#' @export
server_alive <- function(port = default_port(), host = default_host(), protocol = default_protocol(), path = "validate/ping", ...){

  # check if the session is active
  valid <- FALSE
  tryCatch({
    res <- request_server(
      sprintf('%s://%s:%d/%s', protocol, host, port, path),
      body = NULL, method = 'POST', encode = 'json'
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

#' @export
server_kill <- function(host = default_host(), port = default_port(), protocol = default_protocol(), path = 'validate/shutdown'){
  res <- request_server(sprintf('%s://%s:%d/%s', protocol, host, port, path))
  ret <- httr::content(res)
  attr(ret, 'response') <- res
  ret
}




#' @export
start_server <- function(
  host = default_host(),
  port = default_port(),
  settings = system.file("default_settings.yaml", package = 'restbench'),
  protocol = default_protocol(),
  path_validate = "validate/ping",
  make_default = TRUE,
  ...
){
  port <- as.integer(port)
  stopifnot(isTRUE(is.integer(port)))

  if(port == 0){
    port <- findPort(NULL, mustWork = TRUE)
  }

  item <- dipsaus::list_to_fastmap2(list(
    host = host,
    port = port,
    native = FALSE
  ))

  # Check if a server is running
  alive <- FALSE
  if(!portAvailable(port)){
    # host = '127.0.0.1'; port = 7033; protocol = 'http'
    alive <- server_alive(port = port, host = host, protocol = protocol, path = path_validate)
    if(!alive){
      stop("Port: ", port, " is occupied or invalid.")
    }
  }

  # check existing ports
  server_root <- file.path(R_user_dir('restbench', 'cache'), 'servers')
  server_dir <- file.path(server_root, port)

  if(!alive){

    # load the yaml file in case there are errors
    load_yaml(settings)

    # check existing ports
    dir_create2(server_dir)
    # fs <- as.integer(list.dirs(server_root, recursive = FALSE, full.names = FALSE))

    # create new server
    if(get_os() == 'windows'){
      system2(
        command = R.home("Rscript"),
        args = c(
          "--no-save", "--no-restore",
          "--default-packages=utils,restbench",
          "-e",
          sprintf('"restbench:::start_server_internal(host=\'%s\',port=%d,settings=\'%s\')"',
                  host, port, normalizePath(settings, mustWork = TRUE))
        ),
        stdout = file.path(server_dir, 'stdout.log'),
        stderr = file.path(server_dir, 'stderr.log'),
        wait = FALSE, minimized = TRUE, invisible = FALSE
      )
    } else {
      # server_dir <- "/Users/beauchamplab/Library/Caches/org.R-project.R/R/restbench/servers/7033"
      # host = '127.0.0.1'
      # port = 7033
      # settings = system.file("default_settings.yaml", package = 'restbench')
      # protocol = 'http'

      start_script <- file.path(server_dir, "restbench.start.sh")
      file.copy(system.file('bin/start_server.sh', package = 'restbench'), start_script, overwrite = TRUE)
      Sys.chmod(start_script, mode = "0777", use_umask = FALSE)
      system2( start_script, c(
        settings, port, host, protocol, R.home('R')
      ), stdout = FALSE, stderr = FALSE, wait = FALSE)
      # writeLines(sprintf('tempdir(check = TRUE)\nrestbench:::start_server_internal(host=\'%s\',port=%s,settings=\'%s\')',
      #                    host, port, settings), start_script)
      #
      # infile <- file.path(server_dir, "restbench.start.sh")
      # writeLines('', infile)


      # on.exit({ unlink(f) }, add = TRUE)

      # cmd <- sprintf('nohup "%s" --no-save --no-restore -e "restbench:::start_server_internal(host=\'%s\',port=%s,settings=\'%s\')" > "%s" 2> "%s" & disown', R.home('Rscript'), host, port, settings,
      #                normalizePath(file.path(server_dir, 'stdout.log'), mustWork = FALSE),
      #                normalizePath(file.path(server_dir, 'stderr.log'), mustWork = FALSE))
      #
      # writeLines(cmd, f)

    }

    item$native <- TRUE
  }


  if(is.null(.globals$servers[[host]])){
    .globals$servers[[host]] <- list()
  }
  port <- as.character(port)
  if(is.null(.globals$servers[[host]][[port]])){
    .globals$servers[[host]][[port]] <- item
  }

  message(sprintf("A restbench server started at %s://%s:%s", protocol, host, port))

  if(make_default){
    default_host(host)
    default_port(port)
    default_protocol(protocol)
    # options("restbench.default_server" = item)
    message("You have chosen this server to be the default server.")
  }

  invisible(.globals$servers[[host]][[port]])
}

#' @export
ensure_server <- function(host = default_host(), port = default_port(),
                          protocol = default_protocol(), make_default = TRUE,
                          validate = TRUE, validate_sleep = 0.5, validate_maxwait = 30, ...){
  newly_started <- FALSE
  if(!server_alive(port = port, host = host, protocol = protocol, ...)){
    newly_started <- TRUE
    start_server(host, port, protocol = protocol, make_default = FALSE, ...)

    if(validate){
      timeout <- Sys.time() + validate_maxwait
      while(!server_alive(port = port, host = host, protocol = protocol, ...)){
        if(timeout < Sys.time()){
          stop("Cannot create server at ", host, ":", port, "\nPlease manually start server using `restbench::start_server` function.")
        }
        Sys.sleep(validate_sleep)
      }
    }

  }

  if(make_default){
    default_host(host)
    default_port(port)
    default_protocol(protocol)
  }

  invisible(newly_started)
}
