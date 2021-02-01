# Initialize, run once
debug <- getOption('restbench.debug', FALSE)


#* Create new jobs with shared data directory
#* @serializer json
#* @post /new
function(req) {
  # if(debug){ assign('req', req, envir = globalenv())}
  assign('req', req, envir = globalenv())

  restbench <- asNamespace('restbench')
  userid <- restbench$clean_db_entry(req$HEADERS[['restbench.userid']], "[^a-zA-Z0-9]", msg = "Invalid user ID: illegal characters found.")

  # Parse task
  task <- restbench$handler_unpack_task(req)

  # Queue & watch task
  restbench$queue_task(task, userid)

  return(list(message = "Job submitted."))
}


#* @serializer json
#* @post /query
function(req){
  if(getOption('restbench.debug', FALSE)){
    assign('req', req, envir = globalenv())
  }

  userid <- req$HEADERS[["restbench.userid"]]
  status <- req$body$status
  stopifnot(isTRUE(status %in% c("running", "init", "finish", "valid", "all")))

  tbl <- restbench::handler_query_task(userid, status = status)
  tbl
}

