require(restbench)

f <- '~/Desktop/junk/settings.yaml'
conf <- yaml::read_yaml('inst/debug_settings.yaml')
conf$options$debug = FALSE
yaml::write_yaml(conf, f)

# p = restbench::start_server(port = 7033, settings = f)

restbench:::start_server_internal(port = 7034, settings = '~/Desktop/junk/settings.yaml')

# p = restbench::start_server(port = 7033)

restbench:::portAvailable(7033)
restbench:::server_alive(port = 7033)

task <- restbench:::new_task(function(x){
  if(x == 2){
    stop()
  }
  Sys.sleep(1)
  Sys.getpid()
}, x = 1)
task$port <- 7034

res <- task$validate(); res
res <- task$submit(pack = F); res

# restbench:::db_backup(T)
restbench::list_tasks(status = 'all')
restbench::request_task_query('localhost', port = 7034)

task$collect()


task$status()
task$task_name

task$path_submit <- 'validate/shutdown'
task$submit()

task$remove()

