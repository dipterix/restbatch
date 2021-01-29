start_server_internal <- function(
  host = '127.0.0.1',
  port = 7033,
  settings = system.file("default_settings.yaml", package = 'restbench')
){

  # yaml::write_yaml(list(
  #   modules = list(
  #     jobs = '{system.file("scheduler/jobs.R", package = "restbench")}'
  #   ),
  #   options = list(
  #     debug = FALSE
  #   )
  # ), 'inst/default_settings.yaml')


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
    current$filter(name = "validate_auth", expr = validate_auth)
    # validate_auth
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
server_alive <- function(host = '127.0.0.1', port, protocol = 'http'){

  server_root <- file.path(R_user_dir('restbench', 'cache'), 'servers')

  alive <- FALSE

  # check if the session is active
  s <- rand_string()
  task <- new_task(dipsaus::new_function2(alist(x=), {
    c(!!s, Sys.getpid())
  }), x = 1, task_name = "test connection")

  on.exit({
    try({
      task$remove()
    })
  }, add = TRUE, after = TRUE)

  tryCatch({

    # host = '127.0.0.1'; port = 7033; protocol = 'http'
    task$submit(sprintf('%s://%s:%d/jobs/new', protocol, host, port))
    # task$collected <- FALSE

    res <- task$collect()
    res <- res[[1]]

    if(!isTRUE(res[[1]] == s && res[[2]] != as.character(Sys.getpid()))){
      stop("Invalid server.")
    }

    alive <- TRUE

  }, error = function(e){

  })
  alive
}

server_kill <- function(host = '127.0.0.1', port, protocol = 'http'){

}




#' @export
start_server <- function(
  host = '127.0.0.1',
  port = 7033,
  settings = system.file("default_settings.yaml", package = 'restbench'),
  protocol = 'http'
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
    alive <- server_alive(host, port, protocol)
    if(!alive){
      stop("Port: ", port, " is occupied or invalid.")
    }
  }

  # check existing ports
  server_root <- file.path(R_user_dir('restbench', 'cache'), 'servers')
  server_dir <- file.path(server_root, port)

  if(!alive){
    # check existing ports
    dir_create2(server_dir)
    # fs <- as.integer(list.dirs(server_root, recursive = FALSE, full.names = FALSE))
    # create new server
    if(get_os() == 'windows'){
      system2(
        command = R.home("Rscript"),
        args = c(
          "--no-save", "--no-restore",
          "--default-packages=methods,datasets,utils,restbench",
          "-e",
          sprintf('"restbench:::start_server_internal(host=\'%s\',port=%d,settings=\'%s\')"',
                  host, port, normalizePath(settings, mustWork = TRUE))
        ),
        stdout = file.path(server_dir, 'stdout.log'),
        stderr = file.path(server_dir, 'stderr.log'),
        wait = FALSE, minimized = TRUE, invisible = FALSE
      )
    } else {
      system2(
        command = R.home("Rscript"),
        args = c(
          "--no-save", "--no-restore",
          "--default-packages=methods,datasets,utils,restbench",
          "-e",
          sprintf('"restbench:::start_server_internal(host=\'%s\',port=%d,settings=\'%s\')"',
                  host, port, normalizePath(settings, mustWork = TRUE))
        ),
        stdout = file.path(server_dir, 'stdout.log'),
        stderr = file.path(server_dir, 'stderr.log'),
        wait = FALSE
      )
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

  .globals$servers[[host]][[port]]
}

