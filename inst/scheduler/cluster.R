# options("restbench.batch_cluster")

#' Input: reg `batchtools` registry file
#'

workers <- getOption('restbench.max_concurrent_jobs', 1L)
reg$cluster.functions <- batchtools::makeClusterFunctionsSocket(workers, 1)

