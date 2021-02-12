
watch_tasks <- function(release_speed){

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
              # this is server so it's fine to use result_only = TRUE to remove globals file (large) to speed up
              item$task$zip(target = paste0(item$task$task_dir, '.zip'), result_only = TRUE)
            }, seed = TRUE)
            remove_task <- FALSE
          }

        }, error = function(e){
          cat("Error while submitting/packing the task:", item$task$task_name,'\n')
          cat("Error message: ", e$message,'\n')
          print(e$call)

          if(!length(item$packing)){
            # error not from packing the task, schedule error remove
            cat("Removing the task from the server (delete)...",'\n')
            item$task$remove(wait = 0)
          } else {
            cat("Removing the task from the queue, restart it later (defer)...",'\n')
          }
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
        Sys.sleep(0.2)
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
        if(is.null(item) || is.null(item$task)){ return() }
        cat("Starting task:", item$task$task_name, '\n')

        res <- run_task(item$task, userid = item$userid)
        .globals$running[[item$task$task_name]] <- dipsaus::list_to_fastmap2(res)

      })
    }

  }


}

server_unfinished_tasks <- function(n = -1, auto_clean = TRUE, include_init = FALSE){
  conn <- db_ensure(close = FALSE)
  db_lock(conn, 1)
  on.exit({
    db_unlock(conn)
    DBI::dbDisconnect(conn)
  })
  if(include_init){
    res <- DBI::dbGetQuery(conn, 'SELECT * FROM restbatchtasksserver WHERE removed="0" AND (status="0" OR status="-1") ORDER BY status DESC, time_added DESC;')
  } else {
    res <- DBI::dbGetQuery(conn, 'SELECT * FROM restbatchtasksserver WHERE removed="0" AND status="-1" ORDER BY status DESC, time_added DESC;')
  }


  sel <- dir.exists(res$path)
  removed <- res[!sel, ]
  res <- res[sel, ]

  if(auto_clean && nrow(removed)){
    # clean task list

    for(ii in seq_len(nrow(removed))){

      DBI::dbExecute(
        conn, sprintf(
          'UPDATE restbatchtasksserver SET removed=1 WHERE userid="%s" AND name="%s"',
          removed$userid[[ii]], removed$name[[ii]]
        )
      )

    }

  }
  db_unlock(conn)
  DBI::dbDisconnect(conn)
  on.exit({})

  if(n == 0){ return(list()) }
  # restore n tasks
  if(n < 0 || n > nrow(res)){ n <- nrow(res) }

  ret <- list()
  ii <- 1

  # new_task_internal(task_root, task_dir, task_name)

  while(length(ret) <= n && ii <= n){
    try({
      row <- res[ii, ]
      task <- restore_task( task_name = row$name, userid = row$userid, .client = FALSE, .update_db = FALSE )
      if(!is.null(task)){
        ret[[ii]] <- list(
          task = task,
          userid = row$userid
        )
      }
    }, silent = TRUE)
    ii <- ii + 1
  }

  ret

}

watch_later_internal <- function(){
  # cat("Watching tasks\n")
  release_speed <- as.numeric(getOption('restbatch.max_release_tasks_per_second', 10.0)) /
    getOption('restbatch.task_queue_interval', 1)

  tasks_waiting <- .subset2(.globals$tasks, "size")()
  tasks_running <- .subset2(.globals$running, "size")()
  tasks_active <- tasks_waiting + tasks_running

  max_tasks <- getOption("restbatch.max_concurrent_tasks", 1L)


  if(tasks_active < max_tasks / 2){
    if(tasks_active == 0){
      # No task running, checking for inactive projects
      include_init <- TRUE
    } else {
      # check unfinished tasks like canceled
      include_init <- FALSE
    }
    unfinished_tasks <- tryCatch({
      server_unfinished_tasks(
        n = ceiling(max_tasks / 2) - tasks_active, auto_clean = TRUE,
        include_init = include_init
      )
    }, error = function(e){ list() })
    if(length(unfinished_tasks)){
      cat("Low load pressure, resuming previous canceled tasks (", length(unfinished_tasks), ").\n", sep = "")

      lapply(unfinished_tasks, function(item){
        queue_task(item$task, item$userid)
      })
    } else if(tasks_active == 0){
      cat("No task is waiting nor running. \n\n")
      .globals$watchers <- 0
      return()
    }
  }

  tryCatch({
    watch_tasks(release_speed)

    .globals$watchers <- 0
    watch_later()
  }, error = function(e){
    print(e)
    cat(e$message)
    cat("\n------------- see above error message ------------\n")
  })

}

watch_later <- function(){

  if(.globals$paused){
    cat("The queue is paused.\n")
    return()
  }

  if(.globals$watchers == 0){
    .globals$watchers <- .globals$watchers + 1
    later::later(watch_later_internal, delay = getOption('restbatch.task_queue_interval', 1))
  }
}

#' Add a task to a queue
#' @description called by server internally, not supposed to be called directly.
#' @param task a task object
#' @param userid user ID
#' @return None
#' @export
queue_task <- function(task, userid){
  .globals <- get('.globals')
  .globals$tasks$add(list(task = task, userid = userid))

  # Watch task
  if(.globals$watchers == 0){
    watch_later()
  }
  invisible()
}

