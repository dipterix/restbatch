#' Generate script to install the 'restbatch' as Linux service
#' @param save_settings_path where to save the settings file
#' @export
generate_service <- function(
  host = "127.0.0.1",
  port = 7033,
  save_settings_path = "~/.restbatch"
){

  message(
    "You are trying to install `restbatch` as a linux service. Please make sure\n",
    "  1. this is a linux (Ubuntu, Debian)\n",
    "  2. you have sudo access\n",
    "This function will generate a folder at  ",
    save_settings_path,
    "\nPlease edit the settings.yaml file carefully."
  )
  ans <- utils::askYesNo(msg = "Proceed? ", default = FALSE)

  if(!isTRUE(ans)){
    message("Abort.")
    return(invisible())
  }

  dir_create2(save_settings_path)
  dir_create2("~/.config/systemd/user/")

  Sys.chmod(save_settings_path, "0777")

  sf <- function(p){
    system.file("linux", p, package = "restbatch")
  }

  file.copy(sf('restbatch.service'),
            file.path(save_settings_path, 'restbatch.service'),
            overwrite = TRUE)

  bashscr <- file.path(save_settings_path, 'register-restbatch.sh')
  file.copy(sf('register-restbatch.sh'), bashscr, overwrite = TRUE)
  bashscr <- normalizePath(bashscr)
  Sys.chmod(bashscr, mode = "0777")

  settings_path <- file.path(save_settings_path, 'settings.yaml')
  s <- yaml::read_yaml(system.file("default_settings.yaml", package = "restbatch"))
  s$host <- host
  s$port <- port
  s$options$max_concurrent_tasks <- ceiling(future::availableCores() / 2)
  yaml::write_yaml(s, settings_path)
  settings_path <- normalizePath(settings_path, mustWork = TRUE)


  s <- readLines(sf('restbatch.sh'))
  writeLines(s, file.path(save_settings_path, 'restbatch.sh'))

  writeLines(c(
    sprintf('RSCRIPT_PATH="%s"', file.path(R.home(component = "bin"), "Rscript")),
    "# Settings file, string if quoted, or R command if unquoted",
    'RESTBATCH_SETTINGS="/usr/local/etc/restbatch/settings.yaml"'
  ), file.path(save_settings_path, "restbatch.conf"))

  message("\n\nThe setup file has been exported. Please run the following command in shell (bash):")
  cat("bash", bashscr, "\n", sep = " ")


}
