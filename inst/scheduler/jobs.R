# Initialize, run once
debug <- getOption('restbench.debug', FALSE)
safe_run <- function(expr, res, debug = FALSE){
  ret <- tryCatch({ expr }, error = function(e){
    # I'm a teapot, well, the request is improper, change your coffee to your tea
    res$status <- 418
    print(e)
    list(
      error = e$message,
      call = paste(utils::capture.output({ e$call }), collapse = '\n')
    )
  })
  ret
}


#* Create new jobs with shared data directory
#* @serializer json
#* @post /new
function(req) {
  if(debug){ assign('req', req, envir = globalenv())}
  safe_run({
    restbench <- asNamespace('restbench')
    userid <- restbench$clean_db_entry(req$HEADERS[['restbench.userid']], "[^a-zA-Z0-9]", msg = "Invalid user ID: illegal characters found.")

    # Parse task
    task <- restbench$handler_unpack_task(req)

    # Queue & watch task
    restbench$queue_task(task, userid)

    return(list(message = "Job submitted."))
  }, res = res, debug = debug)
}


#* @serializer json
#* @post /list
function(req){
  if(debug){ assign('req', req, envir = globalenv())}

  safe_run({
    userid <- req$HEADERS[["restbench.userid"]]
    status <- req$body$status
    stopifnot(isTRUE(status %in% c("running", "init", "finish", "valid", "all", "canceled")))

    tbl <- restbench::handler_query_task(userid, status = status)
    tbl
  }, res = res, debug = debug)

}

#* @serializer json
#* @post /status
function(req, res){
  if(debug){ assign('req', req, envir = globalenv())}

  safe_run({
    ns <- asNamespace('restbench')

    userid <- ns$clean_db_entry(req$HEADERS[["restbench.userid"]], msg = '[1] Invalid user ID')
    task_name <- ns$clean_db_entry(req$body$task_name, disallow = '[^a-zA-Z0-9-_]', msg = '[5] Invalid task name')

    # get database connection and ensure proper disconnection
    conn <- ns$db_ensure(close = FALSE)
    on.exit({ DBI::dbDisconnect(conn) })

    # query the task status
    # userid and task_name only contain letters, digits and -_, so quote them and it's safe against SQL injection
    tbl <- DBI::dbGetQuery(conn, sprintf(
      'SELECT * FROM restbenchtasksserver WHERE userid="%s" AND name="%s"',
      userid, task_name
    ))

    # just hide some information
    tbl$path <- NULL
    tbl$clientip <- NULL
    tbl$ncpu <- NULL
    tbl$userid <- NULL
    tbl
  }, res = res, debug = debug)
}


