require(httr)
require(batchtools)
source("auth.R")

get_task_root <- function(){
  task_path <- raveio::raveio_getopt('task_root')
  if(length(task_path) != 1 || is.na(task_path)){
    task_path <- file.path(raveio::raveio_getopt('tensor_temp_path'), 'rave_workers')
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
    suggested_workers = raveio::raveio_getopt("max_worker")
  ))
}

new_task <- function(fun, ..., task_name){
  if(missing(task_name)){
    task_name <- "noname"
  }
  task_dir <- get_task_path(task_name)
  task_name <- attr(task_dir, 'task_name')
  task_root <- attr(task_dir, 'task_root')
  suppressMessages({

    ..env <- environment()

    reg <- batchtools::makeRegistry(file.dir = task_dir, work.dir = task_root,
                             namespaces = dipsaus:::attached_packages(), make.default = FALSE)
    batchtools::batchMap(fun, ..., reg = reg)

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
      ensure_registry()

      # load jobs
      s <- status()
      if(s$submitted < task$njobs){
        return(FALSE)
      }
      if(s$running > 0){
        return(FALSE)
      }else {
        return(TRUE)
      }
    }
    collect <- function(wait = FALSE, output = NULL){
      if(task$collected){
        return(task$results)
      }
      ensure_registry()
      if(!resolved() && wait){
        batchtools::waitForJobs(stop.on.error = TRUE, reg = reg)
      }
      jc <- batchtools::makeJobCollection(reg = reg)
      batchtools::doJobCollection(jc, output = output)
      res <- lapply(reg$status$job.id, function(jid){
        batchtools::loadResult(jid, reg = reg)
      })
      task$results <- res
      task$collected <- TRUE
      res
    }
    clear <- function(){
      ensure_registry(TRUE)
      batchtools::clearRegistry(reg)
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
      clear = clear
    ))
    task
  })

}

# create a task
task_name <- "test"



# Add the job(s)
FUN <- function(x){
  if(x == 9){
    Sys.sleep(100)
  }
  if(x == 1){
    stop()
  }
  c(x, Sys.getpid())
}
task <- new_task(FUN, x = 1:8, task_name = 'test')

resp <- task$submit(url = 'http://127.0.0.1:7516/new')
httr::content(resp)
task$status()
task$resolved()
task$reg$status
# task$collect()
#
# batchtools::reduceResultsList(reg = task$reg)
#
# # Send the job
#
# reg <- loadRegistry(task$task_dir, make.default = FALSE, writeable = TRUE)
# batchtools::getJobStatus(reg = reg)
# batch
