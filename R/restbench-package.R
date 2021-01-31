## usethis namespace: start
#' @useDynLib restbench, .registration = TRUE
## usethis namespace: end
NULL


.onUnload <- function (libpath) {
  library.dynam.unload("restbench", libpath)
}
