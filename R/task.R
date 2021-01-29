get_task_root <- function(){
  task_path <- restbench_getopt('task_root')
  if(length(task_path) != 1 || is.na(task_path)){
    task_path <- "~/rave_data/cache_dir/restbench"
  }
  if(!dir.exists(task_path)){
    dir.create(task_path, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  }
  normalizePath(task_path)
}
get_task_path <- function(task_name){
  task_root <- get_task_root()
  task_dir <- file.path(task_root, task_name)
  if(dir.exists(task_dir)){
    task_name <- sprintf("%s__%s", task_name, rand_string(16))
    task_dir <- file.path(task_root, task_name)
  }
  attr(task_dir, 'task_name') <- task_name
  attr(task_dir, 'task_root') <- task_root
  task_dir
}

prepare_request <- function(){
  uid <- get_user()
  keys <- private_key(uid)

  time <- get_timeStamp()
  tokens <- sapply(keys, function(key){
    encrypt_string(time, key = key)
  })
  dipsaus::list_to_fastmap2(list(
    auth = list(
      userid = uid,
      username = get_username(),
      tokens = tokens
    ),
    timeStamp = time,
    suggested_workers = restbench_getopt("max_worker")
  ))
}

new_task_internal <- function(task_root, task_dir, task_name, reg){
  suppressMessages({

    ..env <- environment()

    if(missing(reg)){
      reg <- batchtools::loadRegistry(task_dir, make.default = FALSE, writeable = TRUE)
    }

    ensure_registry <- function(writeable = FALSE){
      suppressMessages({
        ..env$reg <- batchtools::loadRegistry(task_dir, make.default = FALSE, writeable = writeable)
      })
      task$reg <- ..env$reg
      task$reg
    }
    status <- function(){
      ensure_registry()
      batchtools::getStatus(reg = reg)
    }
    submit <- function(url = 'http://127.0.0.1:7033/jobs/new'){
      body <- prepare_request()

      body$path <- task_dir
      body$task_name <- task_name

      res <- httr::POST(
        url = url,
        encode = 'json',
        body = as.list(body)
      )
      # httr::has_content(res)
      # httr::content(res)
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
      if(s$done + s$error + s$expired >= task$njobs && s$running == 0){
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

    task <- dipsaus::list_to_fastmap2(list(
      reg = reg,
      task_name = task_name,
      task_dir = task_dir,
      task_root = task_root,
      njobs = nrow(reg$status),
      results = NULL,
      collected = FALSE,

      # methods
      status = status,
      submit = submit,
      resolved = resolved,
      collect = collect,
      clear = clear,
      remove = remove
    ))
    task
  })
}

#' @export
new_task <- function(fun, ..., task_name){
  if(missing(task_name)){
    task_name <- "noname"
  }
  task_dir <- get_task_path(task_name)
  task_name <- attr(task_dir, 'task_name')
  task_root <- attr(task_dir, 'task_root')
  suppressMessages({

    reg <- batchtools::makeRegistry(file.dir = task_dir, work.dir = task_root,
                                    namespaces = attached_packages(), make.default = FALSE)
    batchtools::batchMap(fun, ..., reg = reg)

  })
  new_task_internal(task_root, task_dir, task_name, reg)

}

#' @export
restore_task <- function(task_name){
  task_root <- get_task_root()
  task_dir <- file.path(task_root, task_name)
  if(!dir.exists(task_dir)){ return(NULL) }

  tryCatch({
    new_task_internal(task_root, task_dir, task_name)
  }, error = function(e){
    NULL
  })
}

# # create a task
# task_name <- "test"
#
#
#
# # Add the job(s)
# FUN <- function(x){
#   if(x == 9){
#     Sys.sleep(100)
#   }
#   if(x == 1){
#     stop()
#   }
#   c(x, Sys.getpid())
# }
# task <- new_task(FUN, x = 1:8, task_name = 'test')
#
# resp <- task$submit(url = 'http://127.0.0.1:7033/jobs/new')
# httr::content(resp)
# task$status()
# task$resolved()
# task$reg$status
# task$collect()
