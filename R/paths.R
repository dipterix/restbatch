get_task_root <- function(){
  task_path <- getOption("restbench.task_root", default = restbench_getopt('task_root'))
  if(length(task_path) != 1 || is.na(task_path)){
    task_path <- "~/rave_data/cache_dir/restbench"
  }
  if(!dir.exists(task_path)){
    dir.create(task_path, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  }
  normalizePath(task_path)
}

get_task_path <- function(task_name, asis = FALSE, userid = get_user()){
  task_name <- clean_db_entry(
    task_name, "[^A-Za-z0-9-_]",
    msg = sprintf("[1] Invalid task name [%s]. Can only contains letters, digits, and `-`, `_`", task_name))
  # userid <- get_user()
  if(!startsWith(task_name, paste0(userid, '__'))){
    task_name <- sprintf("%s__%s", userid, task_name)
  }

  task_root <- get_task_root()
  task_dir <- file.path(task_root, task_name)
  if(!asis && dir.exists(task_dir)){
    task_name <- sprintf("%s__%s", task_name, rand_string(16))
    task_dir <- file.path(task_root, task_name)
  }
  attr(task_dir, 'task_name') <- task_name
  attr(task_dir, 'task_root') <- task_root
  task_dir
}

