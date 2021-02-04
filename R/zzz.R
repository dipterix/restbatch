# To pass checks and to prevent failures
# In the run time, .globals will be replaced with fastmap2
.globals <- new.env()

.onLoad <- function(libname, pkgname){
  pkg <- getNamespace(pkgname)
  sess_str <- rand_string(15)
  assign('.session_string', sess_str, envir = pkg)
  s <- load_setting(reset_temp = TRUE)
  assign('.settings', s, envir = pkg)

  .globals <- dipsaus::fastmap2()
  .globals$servers <- dipsaus::fastmap2()
  .globals$tasks <- fastmap::fastqueue()
  .globals$running <- dipsaus::fastmap2()
  .globals$paused <- TRUE
  .globals$watchers <- 0
  .globals$sql_conn <- NULL
  assign('.globals', .globals, envir = pkg)
}

#' @export
print.restbatch.result <- function(x, ...){
  task <- attr(x, 'task')
  cat(sprintf("Results from task [%s]\n", task$task_name))
  print(task$local_status())
  cat("Display details:\n\n")
  attributes(x) <- NULL
  print(x)
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


