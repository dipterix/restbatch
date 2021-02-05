db_validate <- function(conn){

  # get and check the table
  tryCatch({

    if(missing(conn)){
      conn <- db_ensure(close = FALSE)
      on.exit({
        try({
          DBI::dbDisconnect(conn)
        }, silent = TRUE)
      })
    }

    # check restbatchlocker
    tbl <- DBI::dbReadTable(conn, "restbatchlocker")
    stopifnot(setequal(names(tbl), c("locked", "timeStamp", "lockedBy")))
    stopifnot(is.integer(tbl$locked))
    stopifnot(is.numeric(tbl$timeStamp))
    stopifnot(is.character(tbl$lockedBy))

    # check restbatchuser
    tbl <- DBI::dbReadTable(conn, "restbatchuser")
    stopifnot(setequal(names(tbl), c("userid", "username", "private_key", "public_key", "date_added", "role")))
    stopifnot(is.character(tbl$userid))
    stopifnot(is.character(tbl$username))
    stopifnot(is.character(tbl$private_key))
    stopifnot(is.character(tbl$public_key))
    stopifnot(is.numeric(tbl$date_added))
    stopifnot(is.character(tbl$role))

    # check restbatchtasksclient
    res <- DBI::dbSendQuery(conn, "SELECT * FROM restbatchtasksclient LIMIT 1;")
    tbl <- DBI::dbFetch(res)
    DBI::dbClearResult(res)
    stopifnot(setequal(names(tbl),
                       c("name", "userid", "submitted", "collected", "error", "path",
                         "serverip", "serverport", "removed", "time_added")))
    if(nrow(tbl)){
      stopifnot(is.character(tbl$name))
      stopifnot(is.character(tbl$userid))
      stopifnot(is.character(tbl$path))
      stopifnot(is.character(tbl$serverip))
      stopifnot(is.integer(tbl$serverport))
      stopifnot(is.integer(tbl$submitted))
      stopifnot(is.integer(tbl$collected))
      stopifnot(is.integer(tbl$error))
      stopifnot(is.integer(tbl$removed))
      stopifnot(is.numeric(tbl$time_added))
    }

    # check restbatchtasksserver
    res <- DBI::dbSendQuery(conn, "SELECT * FROM restbatchtasksserver LIMIT 1;")
    tbl <- DBI::dbFetch(res)
    DBI::dbClearResult(res)
    stopifnot(setequal(names(tbl),
                       c("name", "userid", "status", "error", "removed", "packed", "path",
                         "ncpu", "clientip", "time_added")))
    if(nrow(tbl)){
      stopifnot(is.character(tbl$name))
      stopifnot(is.character(tbl$userid))
      stopifnot(is.character(tbl$path))
      stopifnot(is.character(tbl$clientip))
      stopifnot(is.integer(tbl$status))
      stopifnot(is.integer(tbl$packed))
      stopifnot(is.integer(tbl$error))
      stopifnot(is.integer(tbl$removed))
      stopifnot(is.numeric(tbl$time_added))
    }

    TRUE

  }, error = function(e){ FALSE })

}

#' @export
database_fix <- function(){
  if(!interactive()){
    stop("`fix_database` must run in an interactive mode")
  }
  valid <- db_validate()
  if(valid){
    message("The database is valid. No need to fix")
    return(invisible())
  }

  if(!isTRUE(utils::askYesNo("An error found in the database, fix? "))){
    message("Aborted.")
    return(invisible())
  }

  # Back up
  message("Backing up previous database")
  bkup <- db_backup(drop = TRUE)
  if(!is.null(bkup)){
    message("The previous database has been moved to: \n\t", normalizePath(bkup))
  }

  message("Initializing new tables...")
  db_ensure(TRUE)

  message("Done.")
  if(length(bkup)){
    if(!isTRUE(utils::askYesNo(paste(c(
      "Do you want to migrate previous database to the new one?",
      "(I'll try to move as much as possible)"
    ), collapse = " ")))){
      message("Finished. If you want to move merge the previous database, run:")
      message(sprintf('  restbatch::database_restore("%s")', bkup))
      return(invisible())
    }
    database_restore(bkup, backup_current = FALSE)
  }
}

#' @export
database_restore <- function(bkup, backup_current = TRUE, tables = c('user', 'client-task', 'server-task'),
                             owner_key = c('drop_old', 'keep_both')){

  if(!interactive()){
    stop("`fix_database` must run in an interactive mode")
  }
  owner_key <- match.arg(owner_key)


  db_old <- file.path(bkup, 'DB', "restbatch.sqlite")
  if(dir.exists(db_old)){

    if(backup_current){
      message("Old database found. Backing up current database")
      bkup2 <- db_backup(drop = FALSE)
      if(!is.null(bkup)){
        message("You current database has been backed up in case restore procedure fails: \n\t", normalizePath(bkup2))
      }
    }


    # read the database

    # open database
    conn_old <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_old)
    conn_current <- db_ensure(close = FALSE)
    on.exit({
      try({ DBI::dbDisconnect(conn_old) }, silent = TRUE)
      try({ DBI::dbDisconnect(conn_current) }, silent = TRUE)
    })

    merges <- c(FALSE, FALSE, FALSE)

    # restbatchuser
    if('user' %in% tables) {
      tbl_old <- DBI::dbReadTable(conn_old, "restbatchuser")
      tbl_current <- DBI::dbReadTable(conn_current, "restbatchuser")

      nms_old <- names(tbl_old)
      nms_current <- names(tbl_current)
      miss_nm <- nms_current[!nms_current %in% c(nms_old, 'role')]
      if(length(miss_nm)){
        stop("Missing columns ", paste0("[", miss_nm, ']', collapse = ", "),
             " in the user table. These are mandatory columns. I cannot auto merge the database.")
      } else {
        merges[[1]] <- TRUE
      }

    }

    # restbatchtasksclient, not implemented
    if('client-task' %in% tables){
      tbl_old <- local({
        res <- DBI::dbSendQuery(conn_old, "SELECT * FROM restbatchtasksclient LIMIT 1;")
        tbl <- DBI::dbFetch(res)
        DBI::dbClearResult(res)
        tbl
      })
      if(nrow(tbl_old)){
        tbl_current <- local({
          res <- DBI::dbSendQuery(conn_current, "SELECT * FROM restbatchtasksclient LIMIT 1;")
          tbl <- DBI::dbFetch(res)
          DBI::dbClearResult(res)
          tbl
        })

        nms_old <- names(tbl_old)
        nms_current <- names(tbl_current)
        miss_nm <- nms_current[!nms_current %in% nms_old]

        if(length(miss_nm)){
          stop("Missing columns ", paste0("[", miss_nm, ']', collapse = ", "),
               " in the client-task table. These are mandatory columns. I cannot auto merge the database.")
        } else {
          merges[[2]] <- TRUE
        }
      }
    }

    # restbatchtasksserver
    if('server-task' %in% tables){
      tbl_old <- local({
        res <- DBI::dbSendQuery(conn_old, "SELECT * FROM restbatchtasksserver LIMIT 1;")
        tbl <- DBI::dbFetch(res)
        DBI::dbClearResult(res)
        tbl
      })
      if(nrow(tbl_old)){
        tbl_current <- local({
          res <- DBI::dbSendQuery(conn_current, "SELECT * FROM restbatchtasksserver LIMIT 1;")
          tbl <- DBI::dbFetch(res)
          DBI::dbClearResult(res)
          tbl
        })

        nms_old <- names(tbl_old)
        nms_current <- names(tbl_current)
        miss_nm <- nms_current[!nms_current %in% nms_old]

        if(length(miss_nm)){
          stop("Missing columns ", paste0("[", miss_nm, ']', collapse = ", "),
               " in the server-task table. These are mandatory columns. I cannot auto merge the database.")
        } else {
          merges[[3]] <- TRUE
        }
      }
    }

    if(merges[[1]]){

      message("Merging users")
      tbl_old <- DBI::dbReadTable(conn_old, "restbatchuser")
      tbl_current <- DBI::dbReadTable(conn_current, "restbatchuser")

      nms_old <- names(tbl_old)
      owner_id <- tbl_current$userid[tbl_current$role %in% c("owner")]
      admin_id <- tbl_current$userid[tbl_current$role %in% c("admin")]

      if(!'role' %in% nms_old) {
        tbl_old$role <- 'user'
      }
      is_owner <- tbl_old$userid %in% owner_id
      is_admin <- tbl_old$admin %in% admin_id
      tbl_old$role[is_admin] <- "admin"

      # owner_key = c('drop_old', 'keep_both', "drop_new")
      if(owner_key == 'drop_old'){
        tbl_old <- tbl_old[!is_owner, ]
      } else {
        tbl_old$role[is_owner] <- "owner"
      }

      if(nrow(tbl_old)){
        # rbind to see if the table can be binded
        rbind(tbl_current, tbl_old)
        DBI::dbAppendTable(conn_current, name = "restbatchuser", tbl_old)
      }
    }

    if(merges[[2]]){

      message("Merging client tasks")
      tbl_old <- local({
        res <- DBI::dbSendQuery(conn_old, "SELECT * FROM restbatchtasksclient WHERE removed=0;")
        tbl <- DBI::dbFetch(res)
        DBI::dbClearResult(res)
        tbl
      })
      if(nrow(tbl_old)){
        DBI::dbAppendTable(conn_current, name = "restbatchtasksclient", tbl_old)
      }
    }

    if(merges[[3]]){

      message("Merging server tasks")
      tbl_old <- local({
        res <- DBI::dbSendQuery(conn_old, "SELECT * FROM restbatchtasksserver WHERE removed=0;")
        tbl <- DBI::dbFetch(res)
        DBI::dbClearResult(res)
        tbl
      })
      if(nrow(tbl_old)){
        tbl_old$status[tbl_old$status != 2] <- -1 # canceled
        DBI::dbAppendTable(conn_current, name = "restbatchtasksserver", tbl_old)
      }
    }

    message("Done.")
  } else {
    stop("The backup file does not exist.")
  }
}

db_backup <- function(drop=FALSE){
  dbdir <- file.path(R_user_dir('restbatch', which = "data"), 'DB')
  dir_create2(dbdir)

  db_file <- file.path(dbdir, "restbatch.sqlite")
  tdir <- NULL
  if(file.exists(db_file)){
    # copy
    tdir <- file.path(R_user_dir('restbatch', which = "data"), strftime(Sys.time(), 'DB.old.%Y%m%d%H%M%S'))
    dir_create2(tdir)
    file.copy(dbdir, tdir, overwrite = TRUE, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)

    if(drop){
      tmpf <- file.path(dbdir, strftime(Sys.time(), "restbatch.old.%Y%m%d%H%M%S.sqlite"))
      file.rename(db_file, tmpf)
      unlink(tmpf, force = TRUE)
    }
  }
  tdir
}
