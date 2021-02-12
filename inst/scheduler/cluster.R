# options("restbatch.batch_cluster")

#' Input: reg `batchtools` registry file
#'

workers <- getOption('restbatch.max_concurrent_jobs', 1L)




### Fixed usage if batchtools is used

# variables available
task_dir <- get("task_dir")
task_root <- get("task_root")

reg <- batchtools::loadRegistry(task_dir, work.dir = task_root, make.default = FALSE, writeable = TRUE)

# replace with parallel package
# batchtools::makeClusterFunctionsSocket(workers, 1)

if("windows" %in% .Platform$OS.type ||
   stringr::str_detect(stringr::str_to_lower(R.version$os), '^win')){
  # windows, use socket

  reg$cluster.functions <- restbatch::makeClusterFunctionsSocket2(workers, 1)

} else {
  # use forked
  reg$cluster.functions <- batchtools::makeClusterFunctionsMulticore(workers, 0)
}


batchtools::sweepRegistry(reg = reg)
batchtools::saveRegistry(reg = reg)

# This step may take time.
batchtools::submitJobs(reg = reg)
# batchtools::submitJobs(reg = reg, ids = 2:4)
batchtools::waitForJobs(reg = reg)

