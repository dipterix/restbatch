task <- restbatch:::new_task(function(x){
  Sys.getpid()
}, x = 1:4)





future::plan(future::multisession)

f <- future::future({
  task$reload_registry(writeable = TRUE)
  makeClusterFunctionsSocket2 <- function (ncpus = NA_integer_, fs.latency = 65) {
    checkmate::assertCount(ncpus, positive = TRUE, na.ok = TRUE)
    if (is.na(ncpus)) {
      ncpus = max(getOption("mc.cores", parallel::detectCores()), 1L, na.rm = TRUE)
    }

    parallel <- asNamespace("parallel")
    self <- new.env(parent = emptyenv())
    self$cl <- parallel$makePSOCKcluster(rep.int("localhost", ncpus))
    self$pids <- character(ncpus)
    reg.finalizer(self, function(e) {
      print("stop clusters")
      if (!is.null(e$cl)) {
        parallel$stopCluster(e$cl)
        e$cl = NULL
      }
    } , onexit = TRUE)

    spawn = function(jc, ...) {
      force(jc)
      if (all(nzchar(self$pids))) {
        res = parallel$recvOneResult(self$cl)
        self$pids[self$pids == res$tag] = ""
      }
      i = checkmate::wf(!nzchar(self$pids))
      parallel$sendCall(
        self$cl[[i]],
        batchtools::doJobCollection,
        list(jc = jc, output = jc$log.file),
        return = FALSE,
        tag = jc$job.hash
      )
      self$pids[i] = jc$job.hash
      invisible(jc$job.hash)
    }

    list_running = function() {
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


    submitJob = function(reg, jc) {
      batchtools::assertRegistry(reg, writeable = TRUE)
      checkmate::assertClass(jc, "JobCollection")
      spawn(jc)
      batchtools::makeSubmitJobResult(status = 0L, batch.id = jc$job.hash)
    }
    listJobsRunning = function(reg) {
      batchtools::assertRegistry(reg, writeable = FALSE)
      list_running()
    }
    batchtools::makeClusterFunctions(
      name = "Socket",
      submitJob = submitJob,
      listJobsRunning = listJobsRunning,
      store.job.collection = FALSE,
      fs.latency = fs.latency,
      hooks = list(
        pre.sync = function(reg, fns){
          list_running()
        }
      )
    )
  }
  task$reg$cluster.functions <- makeClusterFunctionsSocket2(2, 1)
  batchtools::submitJobs(reg = task$reg)
  Sys.sleep(2)
})

task$locally_resolved()
task$remove(); gc()
rm(task); gc()
