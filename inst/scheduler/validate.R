handler_validate_server <- getOption("restbench.func_validate_server", "restbench::handler_validate_server")
if(!is.function(handler_validate_server)){
  handler_validate_server <- eval(parse(text = handler_validate_server))
}

#* Ping and get validated information back from server
#* @serializer json
#* @post /ping
function(req) {
  if(getOption('restbench.debug', FALSE)){
    assign('req', req, envir = globalenv())
  }
  return(handler_validate_server(req))
}


#* Shutdown the server
#* @serializer json
#* @post /shutdown
function(req) {
  if(getOption('restbench.debug', FALSE)){
    assign('req', req, envir = globalenv())
  }
  ns <- asNamespace('restbench')

  uid <- ns$get_user()
  req_header <- as.list(req$HEADERS)
  userid <- req_header$restbench.userid

  if(userid == uid){
    # shutdown
    later::later(function(){
      do.call("quit", list(save = "no"))
    }, delay = 3)
    return(list(message = "Shutting down..."))
  }

  return(list(error = "Invalid user authentication to shutdown the server."))
}
