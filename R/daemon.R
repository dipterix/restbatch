#' Generate script to install the 'restbatch' as Linux service
#' @param save_settings_path where to save the settings file
#' @export
generate_service <- function(save_settings_path = "~/.restbatch"){

  message(
    "You are trying to install `restbatch` as a linux service. Please make sure:\n",
    "  1. this is a linux (Ubuntu, Debian)\n",
    "  2. you are the administrator\n",
    "This function will generate a folder at \n  ",
    save_settings_path,
    "\nPlease edit the settings.yaml file carefully."
  )
  ans <- utils::askYesNo(msg = "Proceed? ", default = FALSE)

  if(!isTRUE(ans)){
    message("Abort.")
    return(invisible())
  }


  dir_create2(save_settings_path)

  Sys.chmod(save_settings_path, "0777")

  sf <- function(p){
    system.file("linux", p, package = "restbatch")
  }

  file.copy(sf('restbatch.service'),
            file.path(save_settings_path, 'restbatch.service'))

  bashscr <- file.path(save_settings_path, 'register-restbatch.sh')
  file.copy(sf('register-restbatch.sh'), bashscr)
  bashscr <- normalizePath(bashscr)
  Sys.chmod(bashscr, mode = "0777")

  file.copy(sf('restbatch.sh'),
            file.path(save_settings_path, 'restbatch.sh'))

  settings_path <- file.path(save_settings_path, 'settings.yaml')
  file.copy(system.file("default_settings.yaml", package = "restbatch"),
            settings_path)

  settings_path <- normalizePath(settings_path, mustWork = TRUE)

  writeLines(c(
    sprintf('RSCRIPT_PATH="%s"', file.path(R.home(component = "bin"), "Rscript")),
    "# Settings file, string if quoted, or R command if unquoted",
    sprintf('RESTBATCH_SETTINGS="%s"', settings_path)
  ))

  message("The setup file has been exported. Please run the following command in shell (bash):")
  cat("sudo bash", bashscr, sep = " ")


}
