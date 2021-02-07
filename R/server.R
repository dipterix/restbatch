
get_ip <- function(get_public = NA, timeout = 3){
  ip <- list(
    available = c('127.0.0.1', '0.0.0.0'),
    public = if(isFALSE(get_public)) { NULL } else { getOption("restbench.public_ip", NULL) }
  )
  try({
    s <- switch (
      get_os(),
      'windows' = {
        s <- system("ipconfig", intern=TRUE)
        s <- stringr::str_extract(s, "IPv4 Address.*[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}.*")
        s <- s[!is.na(s)]
        stringr::str_extract(s, '[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}')
      },
      'darwin' = {
        s <- system("ifconfig 2>&1", intern = TRUE)
        s <- stringr::str_extract(s, "inet.*[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")
        s <- s[!is.na(s)]
        # extract the first one as the second is mask
        stringr::str_extract(s, '[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}')
      }, {
        s <- system("ip addr", intern = TRUE)
        s <- stringr::str_extract(s, "inet.*[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")
        s <- s[!is.na(s)]
        # extract the first one as the second is mask
        stringr::str_extract(s, '[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}')
      }
    )
    ip$available <- c(ip$available, s[!is.na(s)])
  }, silent = TRUE)

  # also use ipify
  if(isTRUE(get_public)){
    ip$public <- getOption("restbench.public_ip", try({
      res <- httr::GET("https://api.ipify.org?format=json", httr::timeout(timeout))
      res <- httr::content(res, encoding = 'UTF-8')
      s <- res$ip
      s <- stringr::str_extract(s, "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")
      s <- s[!is.na(s)]
      options("restbench.public_ip" = s)
      s
    }, silent = TRUE))
  }

  ip$available <- unique(ip$available)
  ip
}

host_is_local <- function(host, include_public = NA, ...){
  host %in% unlist(get_ip(get_public = include_public, ...))
}


watch_tasks <- function(){

  debug <- getOption('restbatch.debug', FALSE)
  release_speed <- as.numeric(getOption('restbatch.max_release_tasks_per_second', 10.0))

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

          if(isTRUE(item$task$..server_packed) && isTRUE(item$packing)){
            # package finished
            cat("Task ", item$task$task_name, " packed! Updating the database.\n")
            remove_task <- TRUE
            ..server_status <- 2L
          } else if(!isTRUE(item$task$..server_packed)) {
            cat("Task ", item$task$task_name, " finished, updating server database\n")
            remove_task <- TRUE
            ..server_status <- 2L
          } else {
            # finished, but need to pack
            cat("Task ", item$task$task_name, " finished, packing the result folder.\n")
            item$packing <- TRUE
            item$future <- future::future({
              item$task$zip(target = paste0(item$task$task_dir, '.zip'))
            })
            remove_task <- FALSE
          }

        }, error = function(e){
          cat("Error while submitting/packing the task:", item$task$task_name,'\n')
          cat("Error message: ", e$message,'\n')
          cat("Removing the task from the queue, set status as 'init'...",'\n')
          remove_task <<- TRUE
          ..server_status <<- -1L
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

    smry <- summarize_server(include_expired = FALSE)
    running_tasks <- smry[['running']]
    max_tasks <- getOption("restbatch.max_concurrent_tasks", 1L)

    if(max_tasks > running_tasks){

      cat("Available slot(s):", max_tasks - running_tasks, ".\n")

      # run the first max_tasks-running_tasks tasks
      tasks <- .globals$tasks$mremove(max_tasks - running_tasks)

      # This is a bad name, will change it later to handler_submittask
      run_task <- getOption('restbatch.func_newjob', "restbatch::run_task")
      if(!is.function(run_task)){
        run_task <- eval(parse(text=run_task))
      }

      lapply(tasks, function(item){
        if(is.null(item)){ return() }
        cat("Starting task:", item$task$task_name, '\n')

        res <- run_task(item$task, userid = item$userid)
        .globals$running[[item$task$task_name]] <- dipsaus::list_to_fastmap2(res)

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

    interval <- getOption('restbatch.task_queue_interval', 1)
    interval <- max(interval, 0.1)
    interval <- min(interval, 10)

    later::later(function(){
      # cat("Watching tasks\n")
      .globals$watchers <- 0

      watch_later()

      tryCatch({
        watch_tasks()
      }, error = function(e){
        print(e)
        cat(e$message)
        cat("\n------------- see above error message ------------")
      })

    }, delay = interval)

    .globals$watchers <- .globals$watchers + 1
  }
}


#' Generate a sample configuration file
#' @param file file path or a connection to write the configurations to.
#' @return None
#' @export
conf_sample <- function(file = stdout()){
  s <- readLines(system.file('default_settings.yaml', package = 'restbatch'))
  writeLines(s, file)
  invisible()
  # yaml::write_yaml(list(
  #   modules = list(
  #     task = "{system.file(\"scheduler/task.R\", package = \"restbatch\")}",
  #     validate = "{system.file(\"scheduler/validate.R\", package = \"restbatch\")}"
  #   ),
  #   options = list(
  #     debug = FALSE,
  #     require_auth = TRUE,
  #     modules_require_auth = "task, validate",
  #     request_timeout = Inf,
  #     task_root = "{restbatch::restbatch_getopt(\"task_root\")}",
  #     max_nodetime = 864000L,
  #     func_newjob = "restbatch:::run_task",
  #     func_validate_server = "restbatch::handler_validate_server"
  #   )), file)
}

# only use it in the server
module_require_auth <- function(module, settings){
  module %in% getOption('restbatch.modules_require_auth_list')
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

    do.call("options", structure(list(val), names = sprintf('restbatch.%s', nm)))
  }
  # Settings to set options on modules need authentication
  modules_require_auth <- unlist(
    stringr::str_split(getOption("restbatch.modules_require_auth",
                                 paste(names(modules), collapse = ',')), "[, ]+"))
  require_auth <- getOption("restbatch.require_auth", TRUE)
  if(!require_auth){
    modules_require_auth <- NULL
  }
  options('restbatch.modules_require_auth_list' = modules_require_auth)
  options("restbatch.settings" = settings)

  # scripts related to server configurations
  startup_script <- glue::glue(server_scripts$startup_script)
  options("restbatch.startup_script" = startup_script[file.exists(startup_script)])

  batch_cluster <- glue::glue(server_scripts$batch_cluster)
  options("restbatch.batch_cluster" = batch_cluster[file.exists(batch_cluster)])


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
  .globals$paused <- FALSE

  on.exit({
    .globals$paused <- TRUE
  }, add = TRUE, after = TRUE)


  tryCatch({
    eval(parse(file = getOption("restbatch.startup_script", NULL)))
  }, error = function(e){
    warning("[settings -> startup_script] is invalid script. Use default setup script")
    future::plan(future::multisession, workers = getOption('restbatch.max_concurrent_tasks', 1L) + 1)
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
      # There are running jobs, stop them (set their status to "cancelled")
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

  plumber::pr_run(local({

    settings <- yaml::read_yaml(settings)
    modules <- settings$modules
    modules_require_auth <- getOption('restbatch.modules_require_auth_list')

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

    options("restbatch.active_server" = current)

    current$setDebug(debug = getOption("restbatch.debug", TRUE))

    current

  }), host = host, port = port)
}

portAvailable <- function(port){
  tryCatch({
    srv <- httpuv::startServer(
      host = default_host(allow0 = FALSE),
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
    port <- as.integer(restbatch_getopt("default_port", default = 7033))
    if(length(port) != 1 || is.na(port) || !is.integer(port)){
      port <- httpuv::randomPort()
    }
    for (i in 1:11) {
      if(!portAvailable(port)){
        port <- httpuv::randomPort()
      } else {
        restbatch_setopt('default_port', port, .save = FALSE)
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
#' \code{kill_server} stops the server as soon as possible. It sends signals to
#' the server to cancel the unfinished tasks and shutdown.
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
#' set up scenarios like debugging, runing on local machines, but will
#' introduce security issues is deployed on public addresses.}
#' \item{modules_require_auth}{which modules require default authentications;
#' use comma to seperate.}
#' \item{request_timeout}{part of authentication system. When default
#' authentications is on, each request header needs to include a timestamp
#' and an encrypted token of that timestamp. The server will block the requests
#' that are too old to avoid someone accidentally "steals" your previous tokens.
#' The \code{request_timeout} defines the maximum time in seconds between
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
#' status queued or running on the server}
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

#' @rdname restbatch-server
#' @export
server_alive <- function(port = default_port(), host = default_host(allow0 = FALSE),
                         protocol = default_protocol(), path = "validate/ping", ...){

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

    # create new server
    if(get_os() == 'windows'){

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
      system2(start_script , sprintf('"%s"', R.home('bin/R')), stdout = FALSE, stderr = FALSE, wait = FALSE)
      # writeLines(sprintf('tempdir(check = TRUE)\nrestbatch:::start_server_internal(host=\'%s\',port=%s,settings=\'%s\')',
      #                    host, port, settings), start_script)
      #
      # infile <- file.path(server_dir, "restbatch.start.sh")
      # writeLines('', infile)


      # on.exit({ unlink(f) }, add = TRUE)

      # cmd <- sprintf('nohup "%s" --no-save --no-restore -e "restbatch:::start_server_internal(host=\'%s\',port=%s,settings=\'%s\')" > "%s" 2> "%s" & disown', R.home('Rscript'), host, port, settings,
      #                normalizePath(file.path(server_dir, 'stdout.log'), mustWork = FALSE),
      #                normalizePath(file.path(server_dir, 'stderr.log'), mustWork = FALSE))
      #
      # writeLines(cmd, f)

    }

    item$native <- TRUE
  }


  if(is.null(.globals$servers[[host]])){
    .globals$servers[[host]] <- dipsaus::fastmap2()
  }
  dipsaus::list_to_fastmap2(item, .globals$servers[[host]][[port]])


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

  if(length(port) != 1 || is.na(port) || !is.integer(port) || port <= 0 || port > 65535){
    stop("Invalid port: must be an integer.")
  }

  if(is.null(.globals$servers[[host]])){
    .globals$servers[[host]] <- dipsaus::fastmap2()
  }
  if(is.null(.globals$servers[[host]][[port]])){
    .globals$servers[[host]][[port]] <- dipsaus::fastmap2()
  }
  item <- .globals$servers[[host]][[port]]
  item$supervise <- isTRUE(auto_close)
  item$native <- FALSE
  item$local <- host_is_local(host)

  reg.finalizer(environment(.subset2(item, "as_list")), function(e){
    if(isTRUE(e$get("supervise"))){
      # Connection could be closed or the https is disabled, or simply don't have
      # the correct right to shut down the server
      try({
        kill_server(host = e$get("host"), port = e$get("port"), protocol = "https")
      }, silent = TRUE)
      try({
        kill_server(host = e$get("host"), port = e$get("port"), protocol = "http")
      }, silent = TRUE)
    }
  })
}

#' @rdname restbatch-server
#' @export
ensure_server <- function(host = default_host(), port = default_port(),
                          protocol = default_protocol(), make_default = TRUE,
                          validate = TRUE, validate_sleep = 0.1, validate_maxwait = 30, timeout = 3, ...){
  newly_started <- FALSE
  if(!server_alive(port = port, host = host, protocol = protocol, timeout = timeout, ...)){
    newly_started <- TRUE
    start_server(host, port, protocol = protocol, make_default = FALSE, ...)

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
