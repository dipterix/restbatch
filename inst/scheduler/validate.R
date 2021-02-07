handler_validate_server <- getOption("restbatch.func_validate_server", "restbatch::handler_validate_server")
if(!is.function(handler_validate_server)){
  handler_validate_server <- eval(parse(text = handler_validate_server))
}
debug <- getOption('restbatch.debug', FALSE)

auth_enabled_modules <- getOption('restbatch.modules_require_auth_list')

#* Ping and get validated information back from server
#* @serializer json
#* @post /ping
function(req) {
  if(debug){assign('req', req, envir = globalenv())}

  return(handler_validate_server(req))
}


#* Shutdown the server
#* @serializer json
#* @post /shutdown
function(req) {
  if(debug){assign('req', req, envir = globalenv())}

  ns <- asNamespace('restbatch')

  signal_stop <- FALSE
  if('validate' %in% auth_enabled_modules){
    uid <- ns$get_user()
    req_header <- as.list(req$HEADERS)
    userid <- req_header$restbatch.userid
    if(userid == uid){
      signal_stop <- TRUE
    }
  } else {
    signal_stop <- TRUE
  }


  if(signal_stop){
    # shutdown
    later::later(function(){
      tryCatch({
        message("Soft-shutdown the server. Stopping and finalizing the web service.")
        httpuv <- asNamespace('httpuv')
        httpuv$.globals$paused <- TRUE
      }, error = function(e){
        # Running in a child session, safe to quit
        httpuv::stopAllServers()
      })
    }, delay = 0.5)
    return(list(message = "Shutting down the server now..."))

  }

  return(list(error = "Invalid user authentication to shutdown the server."))
}
