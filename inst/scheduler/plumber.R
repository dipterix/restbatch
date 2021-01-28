#* @apiTitle Plumber Example API

library(future)
library(batchtools)
library(plumber)

pr_run(local({

  # Loggers
  source("logger.R", local = TRUE)

  # Authenrication module
  source("auth.R", local = TRUE)

  # Construct Router
  root = pr()

  # default modules
  modules <- list(
    jobs = pr("jobs.R")
  )

  current <- root
  for(nm in names(modules)){
    current <- pr_mount(current, sprintf("/%s", nm), modules[[nm]])
  }
  # current$filter(name = "logger", expr = logger)
  current$filter(name = "validate_auth", expr = validate_auth)
  # validate_auth
}), port = 7033)
