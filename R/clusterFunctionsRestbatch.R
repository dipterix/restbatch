makeClusterFunctionsRestbatch = function(
  task_dir, urls, timeout = 2, allow_local = FALSE,
  scheduler.latency = 2, fs.latency = 2) {

  # check if server is alive
  urls <- "http://127.0.0.1:7034/task/new"; timeout = 2

  m <- stringr::str_match(urls, "^(http[s]{0,1})://([^:]+):([0-9]+)")
  active <- apply(m, 1, function(x){
    server_alive(port = as.integer(x[[4]]), host = x[[3]],
                 protocol = x[[2]], timeout = timeout)
  })

  if(!any(active)){
    if(allow_local){
      warning("Cannot find active server. Start a local server")
      hosts <- "127.0.0.1"
      ports <- findPort()
      protocols <- default_protocol()
      ensure_server(make_default = FALSE, supervise = TRUE)
      root_urls <- sprintf("%s://%s:%s", protocols, hosts, ports)
    } else {
      stop("Cannot find any active server. Abort.")
    }
    nactive <- 1L
  } else {
    hosts <- m[active,3]
    ports <- m[active,4]
    protocols <- m[active,2]
    root_urls <- sprintf("%s://%s:%s", protocols, hosts, ports)
    nactive <- sum(active)
  }

  running_list <- dipsaus::fastmap2()
  for(i in seq_len(nactive)){
    running_list[[i]] <- dipsaus::list_to_fastmap2(list(
      idle = TRUE,
      root = root_urls[[i]],
      url = sprintf("%s/task/new", root_urls[[i]]),
      host = hosts[[i]],
      port = as.integer(ports[[i]]),
      protocol = protocols[[i]],
      pack = FALSE,
      task = NULL,
      hash = NULL
    ))
  }

  is_idle <- function(){
    vapply(seq_len(nactive), function(i){
      running_list[[i]]$idle
    }, FALSE)
  }
  resolve_one <- function(){
    while(!any(is_idle())){
      Sys.sleep(2)

      lapply(seq_len(nactive), function(i){
        item <- running_list[[i]]
        if(is.null(item$task) || item$task$resolved()) {
          if(!is.null(item$task)){
            item$task$remove()
          }
          item$task <- NULL
          item$hash <- NULL
          item$idle <- TRUE
        }
      })

    }
  }

  submitJob = function(reg, jc) {
    batchtools::assertRegistry(reg, writeable = TRUE)
    stopifnot(inherits(jc, "JobCollection"))

    # make sure at least one idle
    resolve_one()
    i = which(is_idle())[[1]]
    item <- running_list[[i]]

    # create a task
    item$idle <- FALSE
    item$task <- new_task(function(x){
      batchtools::doJobCollection(jc, jc$log.file)
    }, x = 1, task_name = "internal_redirect")
    item$hash <- jc$job.hash
    item$task$host <- item$host
    item$task$host <- item$port
    item$task$protocol <- item$protocol

    # submit
    ret <- tryCatch({
      item$task$submit(pack = item$pack, force = TRUE)

      batchtools::makeSubmitJobResult(status = 0L, batch.id = item$task$task_name)
    }, error = function(e){
      # cannot submit, server error (shutdown or connection is dead)
      batchtools::makeSubmitJobResult(status = 101L, batch.id = item$task$task_name)
    })

    return(ret)
  }

  killJob = function(reg, batch.id) {
    batchtools::assertRegistry(reg, writeable = TRUE)
    task <- restore_task2(batch.id)
    task$remove(wait = 0L)
  }

  listJobsRunning = function(reg) {
    ret <- lapply(seq_len(nactive), function(i){
      item <- running_list[[i]]
      if(is.null(item$task)){ return() }
      item$task$task_name
    })
    unlist(ret)
  }

  finish_all <- function(...){
    now <- Sys.sleep()
    while(any({ busy <- !is_idle() })){
      Sys.sleep(2)
      idx <- which(busy)
      lapply(idx, function(i){
        item <- running_list[[i]]
        if(is.null(item$task) || item$task$resolved()) {
          if(!is.null(item$task)){
            item$task$remove()
          }
          item$task <- NULL
          item$hash <- NULL
          item$idle <- TRUE
        }
      })
    }
  }

  batchtools::makeClusterFunctions(
    name = "RestbatchNode", submitJob = submitJob, killJob = killJob,
    listJobsRunning = listJobsRunning, store.job.collection = TRUE,
    scheduler.latency = scheduler.latency, fs.latency = fs.latency,
    hooks = list(pre.sync = finish_all))
}
