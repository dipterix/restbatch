clean_db_entry <- function(entry, disallow = "[^a-zA-Z0-9]",
                           msg = "Invalid entry", strict = TRUE){
  entry <- stringr::str_trim(entry)

  if(!isTRUE(entry != '')){
    stop(msg)
  }
  if(length(entry) != 1){
    stop("Invalid entry length.")
  }
  if(isFALSE(stringr::str_detect(entry, disallow))){
    return(entry)
  }
  if(strict){
    stop(msg)
  }
  entry <- stringr::str_remove_all(entry, disallow)

  entry
}

db_init_tables <- function(conn){

  DBI::dbWriteTable(conn, "restbenchlocker", data.frame(
    locked = FALSE,
    timeStamp = as.numeric(Sys.time()),
    lockedBy = ""
  ))

  DBI::dbWriteTable(conn, "restbenchuser", data.frame(
    userid = get_user(),
    username = get_username(),
    private_key = readLines(system.file('default_key', package = 'restbench'), n = 1),
    public_key = readLines(system.file('default_pubkey', package = 'restbench'), n = 1),
    date_added = as.numeric(Sys.time())
  ))

  DBI::dbWriteTable(conn, "restbenchtasksclient", data.frame(
    name = "",
    userid = "",
    submited = TRUE,
    collected = TRUE,
    error = FALSE,
    path = "",
    serverip = "",
    serverport = 7033,
    removed = TRUE,
    time_added = as.numeric(Sys.time())
  ))

  DBI::dbWriteTable(conn, "restbenchtasksserver", data.frame(
    name = "",
    userid = "",
    status = TRUE, # 0: inited, 1: running, 2: completed/error
    error = FALSE,
    removed = TRUE,
    packed = FALSE,
    path = "",
    ncpu = 0,
    clientip = "",
    time_added = as.numeric(Sys.time())
  ))
}

db_backup <- function(drop=FALSE){
  dbdir <- file.path(R_user_dir('restbench', which = "data"), 'DB')
  dir_create2(dbdir)

  db_file <- file.path(dbdir, "restbench.sqlite")
  if(file.exists(db_file)){
    # copy
    tdir <- file.path(R_user_dir('restbench', which = "data"), strftime(Sys.time(), 'DB.old.%Y%m%d%H%M%S'))
    dir_create2(tdir)
    file.copy(dbdir, tdir, overwrite = TRUE, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)

    if(drop){
      tmpf <- file.path(dbdir, strftime(Sys.time(), "restbench.old.%Y%m%d%H%M%S.sqlite"))
      file.rename(db_file, tmpf)
      unlink(tmpf, force = TRUE)
    }
  }
}

dn_conn_ptr <- function(conn){
  paste(utils::capture.output(conn@ptr), collapse = '')
}

db_lock <- function(conn, lock_duration = 0.2, wait = Inf){
  tbl <- DBI::dbListTables(conn)

  conn_name <- dn_conn_ptr(conn)

  if(!"restbenchlocker" %in% tbl){
    DBI::dbWriteTable(conn, "restbenchlocker", data.frame(
      locked = TRUE,
      timeStamp = Sys.time() + lock_duration,
      lockedBy = conn_name
    ))
    return(TRUE)
  } else {
    now <- as.numeric(Sys.time())

    # db <- dplyr::tbl(conn, "restbenchlocker")
    # Replace locker if
    res <- DBI::dbSendQuery(conn, sprintf(
      'UPDATE restbenchlocker SET locked=1 , timeStamp=%.3f , lockedBy="%s" WHERE locked=0 OR lockedBy="%s" OR timeStamp<%.3f;',
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
        'UPDATE restbenchlocker SET locked=1 , timeStamp=%.3f , lockedBy="%s" WHERE locked=0 OR lockedBy="%s" OR timeStamp<%.3f;',
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
  dbdir <- file.path(R_user_dir('restbench', which = "data"), 'DB')
  dir_create2(dbdir)

  db_file <- file.path(dbdir, "restbench.sqlite")

  has_file <- file.exists(db_file)

  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file)

  tbl <- DBI::dbListTables(conn)

  if(!all(c(c("restbenchtasksclient", "restbenchtasksserver", "restbenchuser", "restbenchlocker")) %in% tbl)){
    # wrong db file
    if(has_file){
      if("restbenchlocker" %in% tbl){
        db_lock(conn, 10)
      }
      DBI::dbDisconnect(conn)
      db_backup(drop = TRUE)
    }
    db_init_tables(conn)
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_file)


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
    'SELECT count(*) AS count FROM restbenchlocker WHERE locked=0 OR lockedBy="%s" OR timeStamp<%.3f;',
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
      'UPDATE restbenchlocker SET locked=0 WHERE locked=1 OR lockedBy="%s" OR timeStamp<%.3f;',
      conn_name, now
    ))
    DBI::dbClearResult(res)
  }
  invisible()
}

db_adduser <- function(userid, private_key, username = NULL, overwrite = FALSE, force = FALSE){
  userid <- stringr::str_trim(userid)

  if(!isTRUE(userid != '' && stringr::str_detect(userid, "^[a-zA-Z0-9]+$"))){
    stop("Invalid userid: must ONLY contain letters, LETTERs, and digits")
  }

  conn <- db_ensure(close = FALSE)
  db_lock(conn, lock_duration = 10, wait = Inf)
  on.exit({
    db_unlock(conn)
    DBI::dbDisconnect(conn)
  }, add = TRUE, after = TRUE)

  # get existing user
  res <- DBI::dbSendQuery(conn, sprintf(
    'SELECT * FROM restbenchuser WHERE userid="%s";',
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

  if(nrow(existing_user) > 0){
    if(overwrite){
      res <- DBI::dbSendQuery(conn, sprintf(
        'DELETE FROM restbenchuser WHERE userid="%s";',
        userid
      ))
      DBI::dbGetInfo(res)
      # res <- DBI::dbFetch(res)
      DBI::dbClearResult(res)
    } else {
      # update username is inconsistent

      if(username != existing_user$username[[1]]){

        res <- DBI::dbSendQuery(conn, sprintf(
          'UPDATE restbenchuser SET username="%s" WHERE userid="%s";',
          username, userid
        ))
        DBI::dbGetInfo(res)
        DBI::dbClearResult(res)
      }

    }

  }

  # Add user
  res <- DBI::dbSendQuery(conn, sprintf(
    'INSERT INTO restbenchuser (userid, username, private_key, public_key, date_added) VALUES ("%s", "%s", "%s", "%s", %.0f);',
    userid, username, private_key, pubkey, as.numeric(Sys.time())
  ))
  info <- DBI::dbGetInfo(res)
  DBI::dbClearResult(res)
  invisible(info)
}

db_getuser <- function(userid, unique = FALSE){
  userid <- stringr::str_trim(userid)

  if(!isTRUE(userid != '' && stringr::str_detect(userid, "^[a-zA-Z0-9]+$"))){
    stop("Invalid userid: must ONLY contain letters, LETTERs, and digits")
  }

  conn <- db_ensure(close = FALSE)
  on.exit({
    DBI::dbDisconnect(conn)
  }, add = TRUE, after = TRUE)

  if(unique){
    res <- DBI::dbSendQuery(conn, sprintf(
      'SELECT DISTINCT userid, username, private_key, public_key FROM restbenchuser WHERE userid="%s";',
      userid
    ))
  } else {
    res <- DBI::dbSendQuery(conn, sprintf(
      'SELECT * FROM restbenchuser WHERE userid="%s";',
      userid
    ))
  }
  existing_user <- DBI::dbFetch(res)
  DBI::dbClearResult(res)
  existing_user
}

db_get_task <- function(task_name, userid, client = TRUE, status = c("running", "init", "finish", "valid", "all")){

  if(missing(userid) && client){
    userid <- get_user()
  }
  userid <- clean_db_entry(userid, msg = "userid must only contain letters and digits.")

  if(!missing(task_name)){
    task_name <- clean_db_entry(
      task_name, "[^A-Za-z0-9-_]",
      msg = sprintf("Invalid task name [%s]. Can only contains letters, digits, and `-`, `_`", task_name))
  }
  status <- match.arg(status)

  conn <- db_ensure(close = FALSE)
  on.exit({
    DBI::dbDisconnect(conn)
  }, add = TRUE)



  # get from client
  if(client){
    qry <- switch (
      status,
      'init' = {
        'AND submited=0 AND removed=0'
      },
      'running' = {
        'AND submited=1 AND collected=0 AND removed=0'
      },
      'finish' = {
        'AND submited=1 AND collected=1 AND removed=0'
      },
      'valid' = {
        'AND removed=0'
      }, {
        ""
      }
    )

    if(missing(task_name)){
      res <- DBI::dbSendQuery(conn, sprintf(
        'SELECT * FROM restbenchtasksclient WHERE userid="%s" %s;',
        userid, qry
      ))
    } else {

      res <- DBI::dbSendQuery(conn, sprintf(
        'SELECT * FROM restbenchtasksclient WHERE userid="%s" AND name="%s" %s;',
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
      'valid' = {
        'AND removed=0'
      }, {
        ""
      }
    )

    if(missing(task_name)){
      res <- DBI::dbSendQuery(conn, sprintf(
        'SELECT * FROM restbenchtasksserver WHERE userid="%s" %s;',
        userid, qry
      ))
    } else {

      res <- DBI::dbSendQuery(conn, sprintf(
        'SELECT * FROM restbenchtasksserver WHERE userid="%s" AND name="%s" %s;',
        userid, task_name, qry
      ))
    }

    tbl <- DBI::dbFetch(res)
    DBI::dbClearResult(res)
    return(tbl)

  }

}

db_update_task_client <- function(task){
  # DBI::dbWriteTable(conn, "restbenchtasksclient", data.frame(
  #   name = "",
  #   userid = "",
  #   submited = TRUE,
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

  conn <- db_ensure(close = FALSE)
  db_lock(conn)
  on.exit({
    db_unlock(conn)
    DBI::dbDisconnect(conn)
  })

  has_error <- tryCatch({
    task$status()$error > 0
  }, error = function(e){
    FALSE
  })

  if(nrow(existing)){
    # update
    res <- DBI::dbSendQuery(conn, sprintf(
      'UPDATE restbenchtasksclient SET submited="%d", collected="%d", error="%d", path="%s", serverip="%s", serverport="%d", removed="%d" WHERE userid="%s" AND name="%s";',
      task$submited, task$collected, has_error, task$task_dir, task$host, task$port,
      !dir.exists(task$task_dir), userid, task$task_name
    ))

  } else {
    # insert
    # dput(names(as.data.frame(dplyr::tbl(conn, 'restbenchtasksclient'))))
    res <- DBI::dbSendQuery(conn, sprintf(
      'INSERT INTO restbenchtasksclient ("name", "userid", "submited", "collected", "error", "path", "serverip", "serverport", "removed", "time_added") VALUES ("%s", "%s", "%d", "%d", "%d", "%s", "%s", "%d", "%d", "%.3f");',
      task$task_name, userid, task$submited, task$collected, has_error, task$task_dir, task$host, task$port,
      !dir.exists(task$task_dir), as.numeric(Sys.time())
    ))
  }
  info <- DBI::dbGetInfo(res)
  DBI::dbClearResult(res)
  invisible(info)

}

db_update_task_server2 <- function(task, userid){

  conn <- db_ensure(close = FALSE)
  on.exit({
    DBI::dbDisconnect(conn)
  })

  # assume task exists
  existing <- db_get_task(task_name = task$task_name, userid = userid, client = FALSE, status = 'all')
  if(!nrow(existing)){
    stop("Task not found on the server.")
  }
  has_error <- tryCatch({
    task$status()$error > 0
  }, error = function(e){
    FALSE
  })
  res <- DBI::dbSendQuery(conn, sprintf(
    'UPDATE restbenchtasksserver SET status="%d", packed="%d", error="%d", path="%s", removed="%d" WHERE userid="%s" AND name="%s";',
    task$server_status, task$server_packed, has_error, task$task_dir, !dir.exists(task$task_dir), userid, task$task_name
  ))

  info <- DBI::dbGetInfo(res)
  DBI::dbClearResult(res)
  invisible(info)
}

db_update_task_server <- function(task, req){

  req_headers <- as.list(req$HEADERS)
  userid <- clean_db_entry(entry = req_headers$restbench.userid, strict = FALSE)

  # get task
  existing <- db_get_task(task_name = task$task_name, userid = userid, client = FALSE, status = 'all')

  conn <- db_ensure(close = FALSE)
  db_lock(conn)
  on.exit({
    db_unlock(conn)
    DBI::dbDisconnect(conn)
  })

  has_error <- tryCatch({
    task$status()$error > 0
  }, error = function(e){
    FALSE
  })
  wk <- as.integer(req_headers$restbench.suggested_workers)
  if(!isTRUE(is.integer(wk))){
    wk <- 1
  } else if (wk > restbench_getopt('max_worker', default = 1L)){
    wk <- restbench_getopt('max_worker', default = 1L)
  }

  if(nrow(existing)){
    update_str <- sprintf(
      'UPDATE restbenchtasksserver SET status="%d", packed="%d", error="%d", path="%s", removed="%d" WHERE userid="%s" AND name="%s";',
      task$server_status, task$server_packed, has_error, task$task_dir, !dir.exists(task$task_dir), userid, task$task_name
    )

    # update
    res <- DBI::dbSendQuery(conn, update_str)

  } else {
    # insert
    if(!length(task$server_status)){
      task$server_status <- 0L
    }

    # dput(names(as.data.frame(dplyr::tbl(conn, 'restbenchtasksserver'))))
    res <- DBI::dbSendQuery(conn, sprintf(
      'INSERT INTO restbenchtasksserver ("name", "userid", "packed", "status", "error", "path", "ncpu", "clientip", "removed", "time_added") VALUES ("%s", "%s", "%d", "%d", "%d", "%s", "%d", "%s", "%d", "%.3f");',
      task$task_name, userid, task$server_packed, task$server_status, has_error, task$task_dir, wk, req$REMOTE_ADDR,
      !dir.exists(task$task_dir), as.numeric(Sys.time())
    ))
  }
  info <- DBI::dbGetInfo(res)
  DBI::dbClearResult(res)
  invisible(info)
}

#' @export
list_tasks <- function(status = c("valid", "running", "init", "finish", "all")){
  status <- match.arg(status)
  userid <- get_user()
  db_get_task(userid = userid, status = status, client = TRUE)
}

# Number of running tasks on the local server (server dev-use only)
server_summary <- function(include_expired = TRUE){
  conn <- db_ensure(close = FALSE)
  on.exit({
    DBI::dbDisconnect(conn)
  }, add = TRUE)
  if(include_expired){
    extra_cond <- sprintf('AND time_added>"%.3f"', as.numeric(Sys.time()) - getOption("restbench.max_nodetime", 60*60*24*10))
  }
  res <- DBI::dbSendQuery(conn, sprintf('SELECT count(*) as count FROM restbenchtasksserver WHERE status=1 %s;', extra_cond))
  running <- DBI::dbFetch(res)
  DBI::dbClearResult(res)

  res <- DBI::dbSendQuery(conn, sprintf('SELECT count(*) as count FROM restbenchtasksserver WHERE status=0 %s;', extra_cond))
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
