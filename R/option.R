
default_settings <- function(s = dipsaus::fastmap2()){
  s[['task_root']] <- '~/rave_data/cache_dir/restbench'
  s[['verbose_level']] <- 'DEBUG'

  s[['max_worker']] <- parallel::detectCores() - 1
  s[['max_mem']] <- "1G"

  s
}

validate_settings <- function(s = dipsaus::fastmap2()){
  d <- default_settings()

  # ------------- Temporary tensor path --------------
  tpath <- s[['task_root']]
  if(length(tpath) == 0){
    s[['task_root']] <- d[['task_root']]
  } else if(length(tpath) > 1 || !isTRUE(is.character(tpath))){
    warning('Option task_root is not length 1 character, reset to default')
    s[['task_root']] <- d[['task_root']]
  }

  # ------------- catgl verbose level --------------
  verbose <- s[['verbose_level']]
  verbose <- verbose[verbose %in% c('DEBUG', 'DEFAULT', 'INFO', 'WARNING', 'ERROR', 'FATAL')]
  if(length(verbose) == 0){
    warning('Option verbose_level is not valid. Choices are: ',
            '"DEBUG", "DEFAULT", "INFO", "WARNING", "ERROR", and "FATAL". ',
            'Reset to default.')
    verbose <- d[['verbose_level']]
  }
  s[['verbose_level']] <- verbose[[1]]

  s
}

load_setting <- function(reset_temp = TRUE){
  s <- get0('.settings', ifnotfound = default_settings())
  tmp <- s$..temp
  sess_str <- get('.session_string')
  conf_path <- R_user_dir(package = 'restbench', which = 'config')
  conf_file <- file.path(conf_path, 'settings.yaml')
  if(file.exists(conf_file)){
    load_yaml(conf_file, map = s)
  }
  s$session_string <- sess_str
  if( reset_temp ){
    s$..temp <- list()
  } else {
    s$..temp <- tmp
  }

  validate_settings(s)
  s
}

#' Set/Get 'restbench' option
#' @description Persist settings on local configuration file
#' @param key character, option name
#' @param value character or logical of length 1, option value
#' @param default is key not found, return default value
#' @param all whether to reset all non-default keys
#' @param .save whether to save to local drive, internally used to temporary
#' change option. Not recommended to use it directly.
#' @param cfile file name in configuration path
#' @param temp when saving, whether the key-value pair should be considered
#' temporary, a temporary settings will be ignored when saving; when getting
#' options, setting \code{temp} to false will reveal the actual settings.
#' @return \code{restbench_setopt} returns modified \code{value};
#' \code{restbench_resetopt} returns current settings as a list;
#' \code{restbench_confpath} returns absolute path for the settings file;
#' \code{restbench_getopt} returns the settings value to the given key, or
#' \code{default} if not found.
#' @seealso \code{R_user_dir}
#' @details \code{restbench_setopt} stores key-value pair in local path.
#' The values are persistent and shared across multiple sessions.
#' There are some read-only keys such as \code{"session_string"}. Trying to
#' set those keys will result in error.
#'
#' \code{restbench_getopt} returns value corresponding to the keys. If key is
#' missing, the whole option will be returned.
#'
#' If set \code{all=TRUE}, \code{restbench_resetopt} resets all keys including
#' non-standard ones. However \code{"session_string"} will never reset.
#' @name restbench-option
NULL

#' @rdname restbench-option
#' @export
restbench_setopt <- function(key, value, .save = TRUE){

  stopifnot2(isTRUE(
    mode(value) %in% c('numeric', 'logical', 'character')
  ), msg = 'settings value must be numeric, character or logical')

  if(is.character(value) && length(value) > 1){
    stop('settings value must be length 1 for characters')
  }

  stopifnot2(!key %in% c('session_string'),
             msg = sprintf('Key %s is read-only', sQuote(key)))

  conf_path <- R_user_dir(package = 'restbench', which = 'config')
  conf_file <- file.path(conf_path, 'settings.yaml')
  s <- load_setting(reset_temp = FALSE)

  previous <- s[[key]]
  s[[key]] <- value
  validate_settings(s)

  if( .save ){
    s$..temp[[key]] <- NULL
    s <- as.list(s)
    s <- s[!names(s) %in% c('session_string', '..temp')]
    dir_create2(conf_path)
    save_yaml(s, conf_file)
  } else {
    # temporarily set value and restore previous value because
    s$..temp[[key]] <- s[[key]]
    if(length(previous) && all(!is.na(previous))){
      s[[key]] <- previous
    }
  }

  invisible(value)
}

#' @rdname restbench-option
#' @export
restbench_resetopt <- function(all = FALSE){
  s <- get('.settings')
  if(all){
    nms <- names(s)
    nms <- nms[!nms %in% c('session_string', '..temp')]
    .subset2(s, 'remove')(nms)
  }
  default_settings(s)
  validate_settings(s)

  # remove some temporary settings
  conf_path <- R_user_dir(package = 'restbench', which = 'config')
  conf_file <- file.path(conf_path, 'settings.yaml')

  if(all && file.exists(conf_file)){
    unlink(conf_file)
  } else {
    dir_create2(conf_path)
    save_yaml(s, conf_file)
  }

  # validate again as temporary settings are removed
  validate_settings(s)

  invisible(as.list(s))
}

#' @rdname restbench-option
#' @export
restbench_getopt <- function(key, default = NA, temp = TRUE){
  s <- get('.settings')
  tmp <- s$..temp

  if(missing(key)){
    s <- as.list(s)
    if(temp){
      for(nm in names(tmp)){
        s[[nm]] <- tmp[[nm]]
      }
    }
    return(s)
  }

  if(temp && (key %in% names(tmp))){
    return(tmp[[key]])
  }
  if(.subset2(s, 'has')(key)){
    return(s[[key]])
  }
  default
}

#' @rdname restbench-option
#' @export
restbench_confpath <- function(cfile = 'settings.yaml'){
  d <- R_user_dir('restbench', 'config')
  normalizePath(file.path(d, cfile), mustWork = FALSE)
}

.onLoad <- function(libname, pkgname) {
  # backports::import(pkgname, c("R_user_dir", "deparse1"))
  pkg <- getNamespace(pkgname)
  sess_str <- rand_string(15)
  assign('.session_string', sess_str, envir = pkg)
  s <- load_setting(reset_temp = TRUE)
  assign('.settings', s, envir = pkg)

  cenv <- environment(.subset2(s, 'reset'))


}

# .onUnload <- function(libpath){
#   s <- load_setting(reset_temp = TRUE)
#   sess_str <- get('.session_string')
# }

