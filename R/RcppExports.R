
spawn_process <- function(args) {
    .Call(`_restbench_spawn_process`, args)
}

