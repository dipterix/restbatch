

#* Create a job created by batchtools
#* @serializer json
#* @post /new
function(req) {
  run_job <- getOption("raveio.func_newjob", raveio::run_job)
  return(run_job(req))
}
