require(restbench)

f <- '~/Desktop/junk/settings.yaml'
conf <- yaml::read_yaml('inst/default_settings.yaml')
conf$options$debug = TRUE
conf$options$require_auth = T
yaml::write_yaml(conf, f)
restbench:::db_backup(T)

devtools::load_all();restbench:::db_backup(T);p = restbench::start_server(port = 7033)



# rstudioapi::jobRunScript(local({
#   ff <- tempfile()
#   writeLines("restbench:::start_server_internal(port = 7034, settings = '~/Desktop/junk/settings.yaml')", ff)
#   ff
# }))
# devtools::load_all();restbench:::db_backup(T);restbench:::start_server_internal(port = 7034, settings = '~/Desktop/junk/settings.yaml')

# p = restbench::start_server(port = 7033)

# restbench:::portAvailable(7034)
# restbench:::server_alive(port = 7034)

task <- restbench:::new_task2(function(x){
  if(x == 2){
    stop()
  }
  Sys.sleep(1)
  Sys.getpid()
}, x = 1:10); task
# task$reload_registry(TRUE)
# task$reg$cluster.functions <- batchtools::makeClusterFunctionsSocket(1)
# batchtools::submitJobs(reg = task$reg)
# batchtools::findExpired(reg = task$reg)
#
# task$status()->s; s
# task$collect()
# task$port <- 7034

# res <- task$validate(); res
res <- task$submit(pack = F); httr::content(res)

# restbench:::db_backup(T)
# restbench:::db_get_task(userid = restbench:::get_user(), client = FALSE, status = 'all')
restbench::list_tasks(status = 'all')
restbench::request_task_list('localhost', port = 7033)
# task$..view()

task$collect()


task$local_status()
task$task_name

# task$remove()

res <- request_server('http://127.0.0.1:7033/jobs/status', body = list(task_name = '64d5010ac8f40ebd109b31817f2ccb04__noname__eODgK1F4aToedcFG'))
httr::content(res)

task$server_status()

restbench::request_task_list('localhost', port = 7033)
task <- restbench:::restore_task('64d5010ac8f40ebd109b31817f2ccb04__noname__eODgK1F4aToedcFG')
task$resolved()
task$local_status()

task$..view()
task$collect()

restbench::server_kill(port = 7033)


