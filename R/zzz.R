# To pass checks and to prevent failures
# In the run time, .globals will be replaced with fastmap2
.globals <- new.env()

.onLoad <- function(libname, pkgname){
  pkg <- getNamespace(pkgname)
  sess_str <- rand_string(15)
  assign('.session_string', sess_str, envir = pkg)
  s <- load_setting(reset_temp = TRUE)
  assign('.settings', s, envir = pkg)

  .globals <- dipsaus::fastmap2()
  .globals$servers <- dipsaus::fastmap2()
  assign('.globals', .globals, envir = pkg)
}
