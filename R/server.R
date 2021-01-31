conf_sample <- function(file = stdout()){
  yaml::write_yaml(list(
    modules = list(
      jobs = '{system.file("scheduler/jobs.R", package = "restbench")}',
      validate = '{system.file("scheduler/validate.R", package = "restbench")}'
    ),
    options = list(
      debug = FALSE,
      require_auth = TRUE,
      request_timeout = Inf,
      task_root = '{restbench::restbench_getopt("task_root")}',
      func_newjob = 'restbench::run_job',
      func_validate_server = 'restbench::handler_validate_server'
    )
  ), file)
}

start_server_internal <- function(
  host = '127.0.0.1',
  port = 7033,
  settings = system.file("debug_settings.yaml", package = 'restbench')
){

  plumber::pr_run(local({

    settings <- yaml::read_yaml(settings)
    modules <- settings$modules
    opts <- settings$options

    for(nm in names(opts)){

      val <- opts[[nm]]
      if(is.character(val)){
        val <- glue::glue(val)
      }

      do.call("options", structure(list(val), names = sprintf('restbench.%s', nm)))
    }

    # Loggers
    # source("logger.R", local = TRUE)
    # logger <- function(req){
    #   cat(as.character(Sys.time()), "-",
    #       req$REQUEST_METHOD, req$PATH_INFO, "-",
    #       req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
    #   plumber::forward()
    # }

    # Authenrication module
    # source("auth.R", local = TRUE)

    # Construct Router
    root = plumber::pr()
    current <- root
    for(nm in names(modules)){
      current <- plumber::pr_mount(current, sprintf("/%s", nm), plumber::pr(glue::glue(modules[[nm]])))
    }
    # current$filter(name = "logger", expr = logger)
    if(getOption("restbench.require_auth", TRUE)){
      current$filter(name = "validate_auth", expr = handler_validate_auth)
    }
    # validate_auth
    current
  }), host = host, port = port)
}

portAvailable <- function(port){
  tryCatch({
    srv <- httpuv::startServer(
      host = "127.0.0.1",
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
server_alive <- function(port, host = '127.0.0.1', protocol = 'http', path = "validate/ping", ...){

  # check if the session is active
  task <- new_task(function(x){}, x = 1, task_name = "test connection")
  task$host <- host
  task$port <- port
  task$protocol <- protocol
  task$path_validate <- path

  # host = '127.0.0.1'; port = 7033; protocol = 'http'
  alive <- task$validate()

  on.exit({
    try({
      task$remove()
    })
  }, add = TRUE, after = TRUE)

  alive
}

server_kill <- function(host = '127.0.0.1', port, protocol = 'http'){

}




#' @export
start_server <- function(
  host = '127.0.0.1',
  port = 7033,
  settings = system.file("default_settings.yaml", package = 'restbench'),
  protocol = 'http',
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
    alive <- server_alive(host, port, protocol, path_validate)
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

      f <- tempfile()
      on.exit({
        unlink(f)
      }, add = TRUE)

      cmd <- sprintf('nohup "%s" --no-save --no-restore -e "restbench:::start_server_internal(host=\'%s\',port=%s,settings=\'%s\')" > "%s" 2> "%s" & disown', R.home('Rscript'), host, port, settings,
                     normalizePath(file.path(server_dir, 'stdout.log'), mustWork = FALSE),
                     normalizePath(file.path(server_dir, 'stderr.log'), mustWork = FALSE))

      writeLines(cmd, f)

      system2(command = Sys.which("bash"), sprintf('"%s"', normalizePath(f)), wait = TRUE)
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

  if(make_default){
    options("restbench.default_server" = item)
  }

  message(sprintf("A restbench server started at %s://%s:%s", protocol, host, port))

  .globals$servers[[host]][[port]]
}

#' @export
ensure_server <- function(host, port, ...){
  item <- getOption("restbench.default_server", list())
  if(missing(host)){
    host <- item$host
  }
  if(missing(port)){
    port <- item$port
  }
  if(is.null(host)){
    host <- '127.0.0.1'
  }
  if(!isTRUE(is.integer(port))){
    port <- 7033
  }
  if(!server_alive(host, port, ...)){
    start_server(host, port, ...)
  }
  invisible()
}
