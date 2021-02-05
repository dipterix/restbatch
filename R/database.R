clean_db_entry <- function(entry, disallow = "[^a-zA-Z0-9]",
                           msg = "Invalid entry", strict = TRUE){
  entry <- stringr::str_trim(entry)
  entry <- paste(entry, collapse = '')

  if(!isTRUE(entry != '')){
    warning(msg)
    stop(msg)
  }
  if(isFALSE(stringr::str_detect(entry, disallow))){
    return(entry)
  }
  if(strict){
    warning(msg)
    stop(msg)
  }
  entry <- stringr::str_remove_all(entry, disallow)

  entry
}




db_init_tables <- function(conn){

  DBI::dbWriteTable(conn, "restbatchlocker", data.frame(
    locked = FALSE,
    timeStamp = as.numeric(Sys.time()),
    lockedBy = ""
  ))

  # always add local user a key
  keys <- keygen()

  DBI::dbExecute(conn, paste(c(
    "CREATE TABLE restbatchuser (",
    "  userid TEXT NOT NULL, ",
    "  username TEXT NOT NULL, ",
    "  private_key TEXT NOT NULL, ",
    "  public_key TEXT NOT NULL, ",
    "  date_added REAL NOT NULL, ",

    # user, owner, admin (restbatch takes no diff between owner and admin)
    # for public server, tasks should run in docker or other places, not the main
    # server, so
    # if someone has access to the database, then they can edit
    '  role TEXT NOT NULL DEFAULT "user" ',
    ");"
  ), collapse = ''))

  DBI::dbAppendTable(conn, "restbatchuser", data.frame(
    userid = get_user(),
    username = get_username(),
    private_key = keys$private,
    public_key = keys$public,
    date_added = as.numeric(Sys.time()),
    role = "owner"
  ))

  DBI::dbCreateTable(conn, "restbatchtasksclient", data.frame(
    name = "",
    userid = "",
    submitted = 1L,
    collected = 1L,
    error = 0L,
    path = "",
    serverip = "",
    serverport = 0L,
    removed = 1L,
    time_added = 0.01
  ))

  DBI::dbCreateTable(conn, "restbatchtasksserver", data.frame(
    name = "",
    userid = "",
    status = 1L, # 0: inited, 1: running, 2: completed/error
    error = 0L,
    removed = 1L,
    packed = 0L,
    path = "",
    ncpu = 0,
    clientip = "",
    time_added = 0.01
  ))
}

dn_conn_ptr <- function(conn){
  paste(utils::capture.output(conn@ptr), collapse = '')
}

db_lock <- function(conn, lock_duration = 0.2, wait = Inf){
  tbl <- DBI::dbListTables(conn)

  conn_name <- dn_conn_ptr(conn)

  if(!"restbatchlocker" %in% tbl){
    DBI::dbWriteTable(conn, "restbatchlocker", data.frame(
      locked = TRUE,
      timeStamp = Sys.time() + lock_duration,
      lockedBy = conn_name
    ))
    return(TRUE)
  } else {
    now <- as.numeric(Sys.time())

    # db <- dplyr::tbl(conn, "restbatchlocker")
    # Replace locker if
    res <- DBI::dbSendQuery(conn, sprintf(
      'UPDATE restbatchlocker SET locked=1 , timeStamp=%.3f , lockedBy="%s" WHERE locked=0 OR lockedBy="%s" OR timeStamp<%.3f;',
      now + lock_duration, conn_name, conn_name, now
    ))
    info <- DBI::dbGetInfo(res)
    DBI::dbClearResult(res)

    if(info$rows.affected > 0){
      return(TRUE)
    }

    while(info$rows.affected == 0){

      if(as.numeric(Sys.time()) - now > wait){
        stop("Acquire locker timeout.")
        break
      }
      res <- DBI::dbSendQuery(conn, sprintf(
        'UPDATE restbatchlocker SET locked=1 , timeStamp=%.3f , lockedBy="%s" WHERE locked=0 OR lockedBy="%s" OR timeStamp<%.3f;',
        as.numeric(Sys.time()) + lock_duration, conn_name, conn_name, as.numeric(Sys.time())
      ))
      info <- DBI::dbGetInfo(res)
      DBI::dbClearResult(res)

      if(info$rows.affected > 0){
        return(TRUE)
      }

      Sys.sleep(0.05)
    }

    return(info$rows.affected > 0)
  }

}

db_ensure <- function(close = FALSE){
  dbdir <- file.path(R_user_dir('restbatch', which = "data"), 'DB')
  dir_create2(dbdir)

  db_file <- file.path(dbdir, "restbatch.sqlite")

  has_file <- file.exists(db_file)

  if(is.null(.globals$sql_conn)){
    .globals$sql_conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file)
  }

  tbl <- tryCatch({
    DBI::dbListTables(.globals$sql_conn)
  }, error = function(e){
    suppressWarnings({
      DBI::dbDisconnect(.globals$sql_conn)
    })
    .globals$sql_conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file)
    DBI::dbListTables(.globals$sql_conn)
  })

  conn <- .globals$sql_conn

  reinit <- FALSE

  if(!isTRUE(.globals$db_valid)){
    .globals$db_valid <- db_validate(conn = conn)
  }
  if(!isTRUE(.globals$db_valid)){

    .globals$db_valid <- TRUE

    # wrong db file
    if(has_file){
      if("restbatchlocker" %in% tbl){
        db_lock(conn, 10)
      }
      DBI::dbDisconnect(conn)
      .globals$db_bkup <- db_backup(drop = TRUE)

      .globals$sql_conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file)
      conn <- .globals$sql_conn
    }
    reinit <- TRUE
  }

  if(reinit){
    db_init_tables(conn)
  }

  if(close){
    DBI::dbDisconnect(conn)
  }
  conn
}

db_locked <- function(conn){
  # Assume sure table exists
  conn_name <- dn_conn_ptr(conn)
  now <- as.numeric(Sys.time())

  res <- DBI::dbSendQuery(conn, sprintf(
    'SELECT count(*) AS count FROM restbatchlocker WHERE locked=0 OR lockedBy="%s" OR timeStamp<%.3f;',
    conn_name, now
  ))
  info <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  if(info$count == 1){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

db_unlock <- function(conn){
  if(!db_locked(conn)){
    now <- as.numeric(Sys.time())
    conn_name <- dn_conn_ptr(conn)
    res <- DBI::dbSendQuery(conn, sprintf(
      'UPDATE restbatchlocker SET locked=0 WHERE locked=1 OR lockedBy="%s" OR timeStamp<%.3f;',
      conn_name, now
    ))
    DBI::dbClearResult(res)
  }
  invisible()
}

db_adduser <- function(userid, private_key, role = c("user", "admin"),
                       username = NULL, overwrite = FALSE, force = FALSE){
  role <- match.arg(role)
  userid <- stringr::str_trim(userid)

  if(!isTRUE(userid != '' && stringr::str_detect(userid, "^[a-zA-Z0-9]+$"))){
    stop("Invalid userid: must ONLY contain letters, LETTERs, and digits")
  }

  conn <- db_ensure(close = FALSE)
  db_lock(conn, lock_duration = 10, wait = Inf)
  on.exit({
    db_unlock(conn)
    Sys.sleep(0.1)
    DBI::dbDisconnect(conn)
  })

  # get existing user
  res <- DBI::dbSendQuery(conn, sprintf(
    'SELECT * FROM restbatchuser WHERE userid="%s";',
    userid
  ))
  existing_user <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  if(is.null(username)){
    if(nrow(existing_user) > 0){
      username <- existing_user$username[[1]]
    } else {
      stop("username not set, nor found in the database")
    }
  }
  username <- stringr::str_remove_all(username, '[^a-zA-Z0-9-_]')

  if(userid == get_user() && overwrite && (!force || interactive())){
    message("You are trying to overwrite your own private keys. This operation will erase your previous credentials, and might result in failure to login the remote servers you have connected to. Do you want to proceed? Enter `Yes/No/Cancel` to choose from the following options:\n  [Yes] Remove existing credentials and only keep the new one\n  [No]  Keep all the credentials\n  [Cancel] Cancel the operation, exit the function.")
    ans <- utils::askYesNo(msg = 'Default is [No]', default = FALSE)
    if(is.na(ans)){
      message("Aborted inserting new credentials")
      return(invisible())
    }
    if(ans){
      overwrite <- TRUE
      force <- TRUE
    } else {
      overwrite <- FALSE
      force <- TRUE
    }
  }

  if(nrow(existing_user) > 0 && !overwrite && !force){
    stop(sprintf("User [%s] (ID: %s) already exists. \n\tIf you want to replace existing entry, set overwrite=TRUE. \n\tIf you want these two entries coexist, set overwrite=FALSE, and force=TRUE", username, userid))
  }

  # Generate pubkey
  pubkey <- private_to_pubkey(private_key)

  is_owner <- userid == get_user()
  missing_owner <- FALSE
  if(nrow(existing_user) > 0){
    if(overwrite){
      res <- DBI::dbExecute(conn, sprintf(
        'DELETE FROM restbatchuser WHERE userid="%s" AND role<>"owner";',
        userid
      ))
      if(is_owner){
        missing_owner <- TRUE
      }
    } else {
      # update username is inconsistent

      if(username != existing_user$username[[1]]){

        res <- DBI::dbExecute(conn, sprintf(
          'UPDATE restbatchuser SET username="%s" WHERE userid="%s";',
          username, userid
        ))
      }

    }

  }

  # Add user
  res <- DBI::dbExecute(conn, sprintf(
    'INSERT INTO restbatchuser (userid, username, private_key, public_key, date_added, role) VALUES ("%s", "%s", "%s", "%s", %.0f, "%s");',
    userid, username, private_key, pubkey, as.numeric(Sys.time()), role
  ))

  invisible()
}

db_getuser <- function(userid, unique = FALSE){
  userid <- stringr::str_trim(userid)

  if(!isTRUE(userid != '' && stringr::str_detect(userid, "^[a-zA-Z0-9]+$"))){
    stop("Invalid userid: must ONLY contain letters, LETTERs, and digits")
  }

  conn <- db_ensure(close = FALSE)
  on.exit({
    DBI::dbDisconnect(conn)
  })

  if(unique){
    res <- DBI::dbSendQuery(conn, sprintf(
      'SELECT DISTINCT userid, username, private_key, public_key FROM restbatchuser WHERE userid="%s";',
      userid
    ))
  } else {
    res <- DBI::dbSendQuery(conn, sprintf(
      'SELECT * FROM restbatchuser WHERE userid="%s";',
      userid
    ))
  }
  existing_user <- DBI::dbFetch(res)
  DBI::dbClearResult(res)
  existing_user
}

db_get_task <- function(task_name, userid, client = TRUE, status = c("running", "init", "finish", "canceled", "valid", "all"),
                        order = FALSE, expire = 0){

  if(missing(userid) && client){
    userid <- get_user()
  }
  userid <- clean_db_entry(userid, msg = "userid must only contain letters and digits.")

  if(!missing(task_name)){
    task_name <- clean_db_entry(
      task_name, "[^A-Za-z0-9-_]",
      msg = sprintf("[4] Invalid task name [%s]. Can only contains letters, digits, and `-`, `_`", task_name))
  }
  status <- match.arg(status)

  conn <- db_ensure(close = FALSE)
  on.exit({
    DBI::dbDisconnect(conn)
  })

  expire <- as.integer(expire)
  additional_qrystr <- ''
  if(isTRUE(expire > 0)){
    additional_qrystr <- sprintf(' AND time_added>"%.0f"', as.numeric(Sys.time()) - expire)
  }
  if(order){
    additional_qrystr <- paste(additional_qrystr, "ORDER BY time_added DESC")
  }

  # get from client
  if(client){
    qry <- switch (
      status,
      'init' = {
        'AND submitted=0 AND removed=0'
      },
      'running' = {
        'AND submitted=1 AND collected=0 AND removed=0'
      },
      'finish' = {
        'AND submitted=1 AND collected=1 AND removed=0'
      },
      "canceled" = {
        stop("Only server can get tasks that canceled.")
      },
      'valid' = {
        'AND removed=0'
      }, {
        ""
      }
    )
    qry <- paste(qry, additional_qrystr)

    if(missing(task_name)){
      res <- DBI::dbSendQuery(conn, sprintf(
        'SELECT * FROM restbatchtasksclient WHERE userid="%s" %s;',
        userid, qry
      ))
    } else {

      res <- DBI::dbSendQuery(conn, sprintf(
        'SELECT * FROM restbatchtasksclient WHERE userid="%s" AND name="%s" %s;',
        userid, task_name, qry
      ))
    }

    tbl <- DBI::dbFetch(res)
    DBI::dbClearResult(res)
    return(tbl)

  } else {
    qry <- switch (
      status,
      'init' = {
        'AND status=0 AND removed=0'
      },
      'running' = {
        'AND status=1 AND removed=0'
      },
      'finish' = {
        'AND status=2 AND removed=0'
      },
      'canceled' = {
        'AND status="-1" AND removed=0'
      },
      'valid' = {
        'AND removed=0'
      }, {
        ""
      }
    )
    qry <- paste(qry, additional_qrystr)

    if(missing(task_name)){
      res <- DBI::dbSendQuery(conn, sprintf(
        'SELECT * FROM restbatchtasksserver WHERE userid="%s" %s;',
        userid, qry
      ))
    } else {

      res <- DBI::dbSendQuery(conn, sprintf(
        'SELECT * FROM restbatchtasksserver WHERE userid="%s" AND name="%s" %s;',
        userid, task_name, qry
      ))
    }

    tbl <- DBI::dbFetch(res)
    DBI::dbClearResult(res)
    return(tbl)

  }

}

db_update_task_client <- function(task){
  # DBI::dbWriteTable(conn, "restbatchtasksclient", data.frame(
  #   name = "",
  #   userid = "",
  #   submitted = TRUE,
  #   collected = TRUE,
  #   error = FALSE,
  #   path = "",
  #   serverip = "",
  #   removed = TRUE,
  #   time_added = as.numeric(Sys.time())
  # ))
  userid <- get_user()
  # get task
  existing <- db_get_task(task_name = task$task_name, userid = userid, client = TRUE, status = 'all')

  has_error <- tryCatch({
    task$local_status()$error > 0
  }, error = function(e){
    FALSE
  })

  conn <- db_ensure(close = FALSE)
  db_lock(conn)
  on.exit({
    db_unlock(conn)
    Sys.sleep(0.1)
    DBI::dbDisconnect(conn)
  })

  if(nrow(existing)){
    # update
    res <- DBI::dbSendQuery(conn, sprintf(
      'UPDATE restbatchtasksclient SET submitted="%d", collected="%d", error="%d", path="%s", serverip="%s", serverport="%d", removed="%d" WHERE userid="%s" AND name="%s";',
      task$submitted, task$collected, has_error, task$task_dir, task$host, task$port,
      !dir.exists(task$task_dir), userid, task$task_name
    ))

  } else {
    # insert
    # dput(names(as.data.frame(dplyr::tbl(conn, 'restbatchtasksclient'))))
    res <- DBI::dbSendQuery(conn, sprintf(
      'INSERT INTO restbatchtasksclient ("name", "userid", "submitted", "collected", "error", "path", "serverip", "serverport", "removed", "time_added") VALUES ("%s", "%s", "%d", "%d", "%d", "%s", "%s", "%d", "%d", "%.3f");',
      task$task_name, userid, task$submitted, task$collected, has_error, task$task_dir, task$host, task$port,
      !dir.exists(task$task_dir), as.numeric(Sys.time())
    ))
  }
  info <- DBI::dbGetInfo(res)
  DBI::dbClearResult(res)
  invisible(info)

}

db_update_task_server2 <- function(task, userid){

  # assume task exists
  existing <- db_get_task(task_name = task$task_name, userid = userid, client = FALSE, status = 'all')
  if(!nrow(existing)){
    stop("Task not found on the server.")
  }
  has_error <- tryCatch({
    task$local_status()$error > 0
  }, error = function(e){
    FALSE
  })

  conn <- db_ensure(close = FALSE)
  on.exit({
    DBI::dbDisconnect(conn)
  })

  res <- DBI::dbSendQuery(conn, sprintf(
    'UPDATE restbatchtasksserver SET status="%d", packed="%d", error="%d", path="%s", removed="%d" WHERE userid="%s" AND name="%s";',
    task$..server_status, task$..server_packed, has_error, task$task_dir, !dir.exists(task$task_dir), userid, task$task_name
  ))

  info <- DBI::dbGetInfo(res)
  DBI::dbClearResult(res)
  invisible(info)
}

db_update_task_server <- function(task, req){

  req_headers <- as.list(req$HEADERS)
  userid <- clean_db_entry(entry = req_headers$restbatch.userid, strict = FALSE)

  # get task
  existing <- db_get_task(task_name = task$task_name, userid = userid, client = FALSE, status = 'all')

  has_error <- tryCatch({
    task$local_status()$error > 0
  }, error = function(e){
    FALSE
  })
  wk <- task$reg$max.concurrent.jobs

  wk <- as.integer(getOption('restbatch.max_concurrent_jobs'))
  if(!length(wk) || is.na(wk[[1]])){
    wk <- as.integer(task$reg$max.concurrent.jobs)
    if(!length(wk) || is.na(wk[[1]])){
      wk <- 1L
    }
  }
  wk <- wk[[1]]

  if(nrow(existing)){
    # update
    sql_str <- sprintf(
      'UPDATE restbatchtasksserver SET status="%d", packed="%d", error="%d", path="%s", removed="%d" WHERE userid="%s" AND name="%s";',
      task$..server_status, task$..server_packed, has_error, task$task_dir, !dir.exists(task$task_dir), userid, task$task_name
    )
  } else {
    # insert
    sql_str <- sprintf(
      'INSERT INTO restbatchtasksserver ("name", "userid", "packed", "status", "error", "path", "ncpu", "clientip", "removed", "time_added") VALUES ("%s", "%s", "%d", "%d", "%d", "%s", "%d", "%s", "%d", "%.3f");',
      task$task_name, userid, task$..server_packed, task$..server_status, has_error, task$task_dir, wk, req$REMOTE_ADDR,
      !dir.exists(task$task_dir), as.numeric(Sys.time())
    )

  }
  conn <- db_ensure(close = FALSE)
  db_lock(conn)
  on.exit({
    db_unlock(conn)
    Sys.sleep(0.1)
    DBI::dbDisconnect(conn)
  })

  res <- DBI::dbSendQuery(conn, sql_str)
  info <- DBI::dbGetInfo(res)
  DBI::dbClearResult(res)
  invisible(info)
}

#' Query and list all your submitted tasks
#' @param status filter task status on the server, choices are \code{'valid'},
#' \code{'init'} (submitted, waiting to run), \code{'running'} (running
#' task), \code{'finish'} (finished task), and \code{'canceled'} (canceled
#' by the server)
#' @param order whether to order by date submitted (in descending order);
#' default is false
#' @param expire positive number (in seconds) to filter out tasks
#' that have been submitted most recently, or 0 (default) to list tasks
#' regardless of their dates. For example, \code{expire=100} will only list
#' tasks submitted during the past 100 seconds.
#' @return A data frame listing tasks submitted, columns are
#' \describe{
#' \item{\code{name}}{task name (ID)}
#' \item{\code{userid}}{your user ID}
#' \item{\code{submitted}}{1 if submitted and 0 otherwise}
#' \item{\code{collected}}{1 if result has been collected and 0 otherwise}
#' \item{\code{error}}{1 if error occurs and 0 otherwise}
#' \item{\code{path}}{the local directory that stores the task data}
#' \item{\code{serverip}}{server address if the task has been submitted}
#' \item{\code{serverport}}{server port if the task has been submitted}
#' \item{\code{removed}}{1 if the task has been removed and 0 otherwise}
#' \item{\code{time_added}}{UNIX time of time when task is created. Use
#' \code{as.POSIXct(time_added, origin="1970-01-01")} to convert to read-able
#' time; see \code{\link{as.POSIXct}}}
#' }
#' @export
list_tasks <- function(status = c("valid", "running", "init", "finish", "all"), order = FALSE, expire = 0){
  status <- match.arg(status)
  userid <- get_user()
  db_get_task(userid = userid, status = status, client = TRUE, order = order, expire = expire)
}

# Number of running tasks on the local server (server dev-use only)
summarize_server <- function(include_expired = TRUE){
  if(include_expired){
    extra_cond <- ''
  } else {
    extra_cond <- sprintf('AND time_added>"%.3f"', as.numeric(Sys.time()) - getOption("restbatch.max_nodetime", 60*60*24*10))
  }
  conn <- db_ensure(close = FALSE)
  on.exit({ DBI::dbDisconnect(conn) })
  res <- DBI::dbSendQuery(conn, sprintf('SELECT count(*) as count FROM restbatchtasksserver WHERE status=1 %s;', extra_cond))
  running <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  res <- DBI::dbSendQuery(conn, sprintf('SELECT count(*) as count FROM restbatchtasksserver WHERE status=0 %s;', extra_cond))
  waiting <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  c(
    waiting = waiting$count,
    running = running$count
  )
}

# db_lock(conn, 3); con <- db_ensure();
# db_locked(con); db_lock(con)
# db_adduser(userid, private_key, username = NULL, overwrite = TRUE, force = FALSE)
# db_adduser(get_user(), private_key, username = username, overwrite = TRUE)
# db_getuser(get_user(), F)

# db_update_task_client(task)
# db_get_task(task$task_name, status = 'all')
# db_get_task(task$task_name, status = 'valid')

# db_update_task_server(task, req)
# db_get_task(client = FALSE, status = 'all', userid = get_user())
