#* @serializer unboxedJSON
#* @get /results
function(req, res){
  assign('req', req, envir = globalenv())
  assign('res', res, envir = globalenv())

  task_name <- req$args$task_name
  if(length(task_name) != 1){
    res$status <- 400
    return(list(error = "Invalid task name"))
  }

  userid <- stringr::str_sub(task_name, end = 32)

  # get task
  ns <- asNamespace('restbatch')
  task <- ns$restore_task(task_name, userid = userid, .client = FALSE)
  if(is.null(task)){
    res$status <- 400
    return(list(error = "Task does not exist anymore"))
  }

  # resolved?
  if(!task$locally_resolved()){
    res$status <- 503
    return(list(error = "Task is not ready to pick up"))
  }


  ret <- task$..local_results()

  return(ret)
}
