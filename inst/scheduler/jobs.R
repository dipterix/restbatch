

#* Create a job created by batchtools
#* @serializer json
#* @post /new
function(req) {
  run_job <- getOption("restbench.func_newjob", restbench::run_job)
  return(run_job(req))
}
