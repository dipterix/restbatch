
#* Create new jobs with shared data directory
#* @serializer json
#* @post /new
function(req) {
  if(getOption('restbench.debug', FALSE)){
    assign('req', req, envir = globalenv())
  }
  return(restbench::handler_parse_task(req))
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

