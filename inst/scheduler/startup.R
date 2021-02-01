# options("restbench.startup_script")

cat("Using default startup script\n")

pool_size <- getOption('restbench.max_concurrent_tasks', 1L)
cat("Initializing pool according to `max_concurrent_tasks`:", pool_size, "\n")

future::plan(future::multisession, workers = pool_size + 1)
