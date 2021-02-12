# make cluster functions implementing batchtools

new_socket <- function(ncpus){
  parallel <- asNamespace("parallel")
  self <- new.env(parent = emptyenv())
  self$cl <- parallel::makePSOCKcluster(rep.int("localhost", ncpus))
  self$pids <- character(ncpus)
  reg.finalizer(self, function(e) {
    if (!is.null(e$cl)) {
      try({
        parallel::stopCluster(e$cl)
        self$cl = NULL
      }, silent = TRUE)
    }
  }, onexit = TRUE)

  self$spawn <- function(jc, ...) {
    force(jc)
    if (all(nzchar(self$pids))) {
      res = parallel$recvOneResult(self$cl)
      self$pids[self$pids == res$tag] = ""
    }
    i = which(!nzchar(self$pids))[[1]]
    parallel$sendCall(
      self$cl[[i]], batchtools::doJobCollection,
      list(jc = jc, output = jc$log.file),
      return = FALSE, tag = jc$job.hash
    )
    self$pids[i] = jc$job.hash
    invisible(jc$job.hash)
  }

  self$list = function() {
    if (is.null(self$cl))
      return(character(0L))

    sl = lapply(self$cl, function(x) x$con)
    finished = which(socketSelect(sl, write = FALSE, timeout = 1))
    for (i in seq_along(finished)) {
      res = parallel$recvOneResult(self$cl)
      self$pids[self$pids == res$tag] = ""
    }

    self$pids[nzchar(self$pids)]
  }

  self

}

makeClusterFunctionsSocket_alt <- function(ncpus = NA_integer_, fs.latency = 65){
  ncpus <- as.integer(ncpus)
  if (is.na(ncpus)) {
    ncpus = max(getOption("mc.cores", parallel::detectCores()), 1L, na.rm = TRUE)
  }

  p = new_socket(ncpus)
  submitJob = function(reg, jc) {
    batchtools::assertRegistry(reg, writeable = TRUE)
    stopifnot(inherits(jc, "JobCollection"))
    p$spawn(jc)
    batchtools::makeSubmitJobResult(status = 0L, batch.id = jc$job.hash)
  }
  listJobsRunning = function(reg) {
    batchtools::assertRegistry(reg, writeable = FALSE)
    p$list()
  }
  batchtools::makeClusterFunctions(
    name = "Socket2", submitJob = submitJob,
    listJobsRunning = listJobsRunning, store.job.collection = FALSE,
    fs.latency = fs.latency, hooks = list(pre.sync = function(reg, fns) p$list()))

}

#' Make 'batchtools' cluster with 'parallel' package
#' @description An implementation of \code{\link[batchtools]{makeClusterFunctionsSocket}}
#' with native 'parallel' package if 'snow' is not installed
#' @param ncpus,fs.latency see \code{\link[batchtools]{makeClusterFunctionsSocket}}
#' @return A \code{\link[batchtools]{makeClusterFunctions}}.
#' @export
makeClusterFunctionsSocket2 <- function(ncpus = NA_integer_, fs.latency = 65){
  if(dipsaus::package_installed("snow")){
    batchtools::makeClusterFunctionsSocket(ncpus, fs.latency)
  } else {
    makeClusterFunctionsSocket_alt(ncpus, fs.latency)
  }
}


