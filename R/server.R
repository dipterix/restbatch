#' Server control functions
#' @description Start, validate, stop a batch server
#' @param host an 'IPv4' address where to run the server on; default see
#' \code{\link{default_host}}
#' @param port integer, which port the server runs at; default see
#' \code{\link{default_port}}
#' @param protocol 'http' or 'https'; default see
#' \code{\link{default_protocol}}. 'https' needs extra server settings
#' @param settings path to a server configuration file; a sample can be
#' obtained via \code{\link{conf_sample}}; see details
#' @param make_default whether to make the server default? If so, all tasks
#' will be sent to this server by default.
#' @param supervise whether to shutdown the server when current R session
#' expires; default is \code{FALSE}
#' @param auto_close same as \code{supervise}
#' @param validate whether to check server is alive once created, default is
#' \code{TRUE}; see details
#' @param validate_sleep if validation is on, intervals to check alive
#' @param validate_maxwait maximum waiting time in seconds to validate
#' @param timeout 'http' request timeout, default is 3, see details.
#' @param path,path_validate internally used
#' @param wait wait until the server is shut down; default is true.
#' @param ... pass to other methods
#'
#' @return \code{start_server} returns a list; \code{server_alive} returns
#' whether the server can be connected; others return nothing.
#'
#' @details The function \code{ensure_server} is a combination of
#' \code{start_server} and \code{server_alive}: it checks whether the target
#' server is alive first before starting the server. If \code{validate} is true,
#' then the it also wait until the server is completely up and ready to serve.
#'
#' \code{timeout} is the seconds that a request should wait to check if the
#' server is alive. On most UNIX systems, request fail immediately once the
#' server is not available. However on Windows, requests might block the session
#' if the server is unavailable. \code{timeout} is the upper time limit that
#' would take to fail the test. If the server fails to respond before the limit,
#' the test will also fail.
#'
#' \code{kill_server,stop_server} stop the server as soon as possible. They
#' send signals to the server to cancel the unfinished tasks and shutdown.
#' \code{autoclose_server} stops the server when the current R session exits.
#'
#' @section Server Configuration:
#'
#' A server configuration is a file that contains key-value pairs. They are
#' used to tune your running servers on performance, security, and even
#' customize some actions.
#'
#' A sample configuration file can be obtained via \code{\link{conf_sample}}
#'
#' Some values are characters. These characters will be parsed through
#' \code{\link[glue]{glue}} function that evaluates dynamically. The adds
#' more flexibility to your settings.
#' For example, the default \code{startup_script} evaluates
#' \code{system.file("scheduler/startup.R", package = "restbatch")} dynamically,
#' and pass the results as your actual startup script.
#'
#' \describe{
#' \item{startup_script}{path to the R script to run when starting up. For example,
#' setting up pools, load extra settings etc.}
#' \item{batch_cluster}{path to the R script defining cluster functions.
#' See \code{\link[batchtools]{makeClusterFunctions}} on how to set up
#' computational nodes.}
#' \item{task,validate}{R \code{plumber} files to handle new job and
#' validation requests}
#' \item{debug}{yes or no to enable debug mode}
#' \item{require_auth}{whether default authentication is on. The default
#' authentication uses 'openssl' that sends tokens in the request headers.}
#' \item{anonymous_request}{whether to allow default tokens; works for a quick
#' set up scenarios like debugging, running on local machines, but will
#' introduce security issues is deployed on public addresses.}
#' \item{modules_require_auth}{which modules require default authentications;
#' use comma to separate.}
#' \item{keep_alive}{part of authentication system. When default
#' authentications is on, each request header needs to include a timestamp
#' and an encrypted token of that timestamp. The server will block the requests
#' that are too old to avoid someone accidentally "steals" your previous tokens.
#' The \code{keep_alive} defines the maximum time in seconds between
#' the encrypted request time and the actual time when server handles requests}
#' \item{max_concurrent_tasks}{maximum of running tasks allowed}
#' \item{max_concurrent_jobs}{maximum of running jobs for each task}
#' \item{max_release_tasks_per_second}{A loop will be created in the server to
#' regularly check the task status (see \code{task_queue_interval}). The loop
#' blocks the session. If there are multiple tasks finished, releasing them
#' all might be time consuming, \code{max_release_tasks_per_second} controls
#' the maximum number of tasks to be released each
#' \code{task_queue_interval} seconds}
#' \item{task_queue_interval}{intervals in seconds to check and update task
#' status queued or running on the server. Recommended to be at least
#' \code{0.5} to avoid database lock.}
#' \item{task_root}{where to store the task files}
#' \item{max_nodetime}{sometimes a task might run forever or get lost. It
#' will still be in the running status. Set this number to indicate the max
#' time in seconds a job should execute; default is 10 days.}
#' \item{func_newjob}{function to handle new tasks}
#' \item{func_validate_server}{function to validate the server}
#' }
#'
#' @seealso \code{\link{conf_sample}},
#' \code{\link[batchtools]{makeClusterFunctions}}, \code{\link[glue]{glue}}.
#'
#' @examples
#'
#' if(interactive()){
#'
#' # -------------------- Start a server tasks -----------------------
#'
#' # set default host, port to play with
#' default_host("127.0.0.1")
#' default_port(7033)
#'
#' # start a server
#' start_server()
#'
#' server_alive()
#'
#' # alternatively, you can just call
#' ensure_server()
#'
#' # -------------------- Run tasks -----------------------------------
#'
#' task <- new_task2(function(x){
#'   if(x == 2){
#'     stop("I stop no because.")
#'   }
#'   Sys.sleep(5)
#'   Sys.getpid()
#' }, x = 1:3)
#'
#' # Submit and run batch jobs
#' task$submit()
#'
#' # Check
#' task$server_status()
#'
#' # print task
#' print(task)
#'
#' # obtain results
#' task$collect()
#'
#' # get result details
#' attributes(task$collect())
#'
#' # -------------------- An interactive shiny client -----------------
#'
#' source(system.file('dashboards/client/app.R', package = 'restbatch'))
#'
#' # -------------------- Stop the server ------------------------------
#' kill_server()
#'
#' # clean up
#' task$remove()
#'
#' }
#'
#' @name restbatch-server
NULL

# only use it in the server
module_require_auth <- function(module, settings){
  module %in% getOption('restbatch.modules_require_auth_list')
}

isin_server <- function(){
  if(interactive()){
    return(FALSE)
  }
  if(is.null(getOption("restbatch_server_addr", NULL))){
    return(FALSE)
  }
  return(TRUE)
}

start_server_internal <- function(
  host = default_host(),
  port = default_port(),
  settings = system.file("debug_settings.yaml", package = 'restbatch')
){

  default_host(host)
  default_port(port)

  settings <- normalizePath(settings, mustWork = TRUE)
  load_server_settings(settings)
  options("restbatch_server_addr" = list(host = host, port = port))
  .globals$paused <- FALSE

  tryCatch({
    eval(parse(file = getOption("restbatch.startup_script", NULL)))
  }, error = function(e){
    warning("[settings -> startup_script] is invalid script. Use default setup script")
    future::plan(future::multisession, workers = getOption('restbatch.max_concurrent_tasks', 1L) + 1)
  })

  on.exit({
    .globals$paused <- TRUE

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
              'UPDATE restbatchtasksserver SET status="-1" WHERE userid="%s" AND name="%s";',
              item$userid, item$task$task_name
            ))
          }, error = function(e){})

        }
      }

      DBI::dbDisconnect(conn)

    }



    cat("Done\n\n\n")
  })

  # start watcher
  watch_later()

  plumber::pr_run(local({

    settings <- yaml::read_yaml(settings)
    modules <- settings$modules
    modules_require_auth <- getOption('restbatch.modules_require_auth_list')

    # Construct Router
    root <- plumber::pr()
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

    options("restbatch.active_server" = current)

    current$setDebug(debug = getOption("restbatch.debug", TRUE))

    current

  }), host = host, port = port)
}




#' @rdname restbatch-server
#' @export
server_alive <- function(port = default_port(), host = default_host(allow0 = FALSE),
                         protocol = default_protocol(), path = "validate/ping",
                         ...){

  if(host == '0.0.0.0'){
    host <- '127.0.0.1'
  }

  # check if the session is active
  valid <- FALSE
  tryCatch({
    res <- request_server(
      # sprintf('%s://%s:%d/%s', protocol, host, port, path),
      path = path, host = host,
      port = port, protocol = protocol,
      body = NULL, method = 'POST', encode = 'json',
      ...
    )

    # get auth info
    ans <- httr::content(res)

    if(isFALSE(ans$auth_enabled[[1]])){
      # auth disabled
      valid <- TRUE
    } else {

      # check the token
      token <- ans$token

      client_md5 <- token$client_md5[[1]]

      my_keys <- private_key(get_user())
      key <- my_keys[vapply(my_keys, function(key){ as.character(key$pubkey$fingerprint) %in% client_md5 }, FALSE)][[1]]

      valid <- openssl::signature_verify(
        data = charToRaw(token$client_answer[[1]]),
        hash = openssl::sha256,
        sig = as.raw(openssl::bignum(token$server_answer[[1]])),
        pubkey = key
      )
    }

    attr(valid, "response") <- res
  }, error = function(e){
    attr(valid, "error") <- e
  })

  valid

}

#' @rdname restbatch-server
#' @export
kill_server <- function(wait = TRUE, host = default_host(allow0 = FALSE), port = default_port(),
                        protocol = default_protocol(), path = 'validate/shutdown'){
  if(host == '0.0.0.0'){
    host <- '127.0.0.1'
  }
  tryCatch({
    res <- request_server(
      path = path, host = host,
      port = port, protocol = protocol
      # sprintf('%s://%s:%d/%s', protocol, host, port, path)
    )
    ret <- httr::content(res)
    ret <- ret$message[[1]]

    message(ret)

  }, error = function(e){
    e$message <- paste0("Failed to close the server due to:\n", e$message)
    stop(e)
  })

  if(!wait){
    return(invisible(TRUE))
  }

  # wait
  Sys.sleep(1)

  alive <- tryCatch({
    server_alive(port = port, host = host, protocol = protocol, timeout = 3)
  }, error = function(e){ FALSE })

  if(alive){
    warning("Signal sent to the server, but server seems to be still alive.")
  }
  return(invisible(!alive))

}

#' @rdname restbatch-server
#' @export
stop_server <- kill_server

#' @rdname restbatch-server
#' @export
start_server <- function(
  host = default_host(),
  port = default_port(),
  settings = system.file("default_settings.yaml", package = 'restbatch'),
  protocol = default_protocol(),
  path_validate = "validate/ping",
  make_default = TRUE,
  supervise = FALSE,
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
    native = FALSE,
    supervise = supervise,
    local = host_is_local(host)
  ))

  # Check if a server is running
  alive <- FALSE
  if(!portAvailable(port)){
    # host = '127.0.0.1'; port = 7033; protocol = 'http'
    alive <- server_alive(port = port, host = host, protocol = protocol, path = path_validate)
    if(!alive){
      stop("Port: ", port, " at ", host, " is occupied or invalid.")
    }
  }

  # check existing ports
  server_root <- file.path(R_user_dir('restbatch', 'cache'), 'servers')
  server_dir <- file.path(server_root, port)

  if(!alive){

    # load the yaml file in case there are errors
    settings_list <- load_yaml(settings)

    # check existing ports
    dir_create2(server_dir)
    # fs <- as.integer(list.dirs(server_root, recursive = FALSE, full.names = FALSE))

    op_sys <- get_os()

    # create new server
    if(op_sys == 'windows'){

      # server_dir <- "C:\\Users\\zheng\\AppData\\Local\\R\\cache\\R\\restbatch\\servers/7033/"
      # host = '127.0.0.1'
      # port = 7033
      # settings = system.file("default_settings.yaml", package = 'restbatch')

      cmd <- sprintf('restbatch:::start_server_internal(host=\'%s\',port=%d,settings=\'%s\')',
                     host, port, normalizePath(settings, mustWork = TRUE, winslash = "/"))

      f <- tempfile()
      writeLines(cmd, f)

      system2(
        command = normalizePath(R.home("bin\\R.exe")),
        args = c(
          "CMD", "BATCH",
          "--no-save", "--no-restore",
          shQuote(normalizePath(f), type = "cmd2"),
          shQuote(normalizePath(file.path(server_dir, "server.log"),
                                mustWork = FALSE), type = "cmd2")
        ),
        stdout = file.path(server_dir, 'stdout.log'),
        stderr = file.path(server_dir, 'stderr.log'),
        wait = FALSE, minimized = TRUE, invisible = FALSE
      )
    } else {
      # server_dir <- "/Users/beauchamplab/Library/Caches/org.R-project.R/R/restbatch/servers/7033"
      # host = '127.0.0.1'
      # port = 7033
      # settings = system.file("default_settings.yaml", package = 'restbatch')
      # protocol = 'http'

      start_script <- file.path(server_dir, "restbatch.start.sh")
      file.copy(system.file('bin/start_server.sh', package = 'restbatch'),
                file.path(server_dir, "restbatch.start.sh"), overwrite = TRUE)
      file.copy(system.file('bin/start_server.R', package = 'restbatch'),
                file.path(server_dir, "start_server.R"), overwrite = TRUE)
      settings_list$host <- host
      settings_list$port <- port
      settings_list$protocol <- protocol
      save_yaml(settings_list, file.path(server_dir, "settings.yaml"))

      Sys.chmod(start_script, mode = "0777", use_umask = FALSE)

      if(op_sys == 'darwin'){
        # Not sure why osx does not disown the process (hardened?)
        system2("bash", c(start_script, sprintf('"%s"', R.home('bin/R'))), stdout = FALSE, stderr = FALSE, wait = FALSE)
      } else {
        system2(start_script , sprintf('"%s"', R.home('bin/R')), stdout = FALSE, stderr = FALSE, wait = FALSE)
      }

    }

    item$native <- TRUE
  }


  if(is.null(.globals$servers[[host]])){
    .globals$servers[[host]] <- dipsaus::fastmap2()
  }
  if(is.null(.globals$servers[[host]][[port]])){
    .globals$servers[[host]][[port]] <- item
  } else {
    dipsaus::list_to_fastmap2(item, .globals$servers[[host]][[port]])
  }

  message(sprintf("Starting a restbatch server at %s://%s:%s", protocol, host, port))

  if(make_default){
    default_host(host)
    default_port(port)
    default_protocol(protocol)
    # options("restbatch.default_server" = item)
    message("You have chosen this server to be the default server.")
  }
  autoclose_server(host = host, port = port, auto_close = supervise)
  invisible(.globals$servers[[host]][[port]])
}

#' @rdname restbatch-server
#' @export
autoclose_server <- function(host = default_host(), port = default_port(), auto_close = TRUE){
  # fastmap is a list so we cannot register finalizers
  # ensure supervised servers are correctly removed when R session exit

  if(isin_server()){
    stop("Cannot stop server within a task")
  }

  port <- as.integer(port)
  if(length(port) != 1 || is.na(port) || !is.integer(port) || port <= 0 || port > 65535){
    stop("Invalid port: must be an integer.")
  }

  server_list <- getOption("restbatch_serverlist", NULL)

  if(!is.environment(server_list)){
    server_list <- new.env(parent = emptyenv())
    options(restbatch_serverlist = server_list)
  }

  url <- sprintf("%s:%d", host, port)
  if(is.null(server_list[[url]])){
    server_list[[url]] <- new.env(parent = emptyenv())

    reg.finalizer(server_list[[url]], function(e){
      if(isTRUE(e$supervise)){
        message("Stopping `restbatch` server at ", e$host, ":", e$port)
        # Connection could be closed or the https is disabled, or simply don't have
        # the correct right to shut down the server
        try({
          kill_server(host = e$host, port = e$port, protocol = "https")
        }, silent = TRUE)
        try({
          kill_server(host = e$host, port = e$port, protocol = "http")
        }, silent = TRUE)
      }
    }, onexit = TRUE)

  }
  supervise <- isTRUE(auto_close)
  server_list[[url]]$supervise <- supervise
  server_list[[url]]$native <- FALSE
  server_list[[url]]$local <- host_is_local(host)
  server_list[[url]]$host <- host
  server_list[[url]]$port <- port

  if(supervise){
    message(sprintf("Auto-close the server %s:%d at the end of the current session", host, port))
  }

  rm(port, host, auto_close, server_list, supervise)
  return(invisible())
}

#' @rdname restbatch-server
#' @export
ensure_server <- function(host = default_host(), port = default_port(),
                          protocol = default_protocol(), make_default = TRUE, supervise = FALSE,
                          validate = TRUE, validate_sleep = 0.1, validate_maxwait = 30, timeout = 3, ...){
  newly_started <- FALSE
  if(!server_alive(port = port, host = host, protocol = protocol, timeout = timeout, ...)){
    newly_started <- TRUE
    start_server(host, port, protocol = protocol, make_default = FALSE, supervise = supervise, ...)

    # wait for the server to get ready
    Sys.sleep(1)
    if(validate){
      expire <- Sys.time() + validate_maxwait
      while(!server_alive(port = port, host = host, protocol = protocol, timeout = timeout, ...)){
        # print("No")
        if(expire < Sys.time()){
          stop("Cannot create server at ", host, ":", port, "\nPlease manually start server using `restbatch::start_server` function.")
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
