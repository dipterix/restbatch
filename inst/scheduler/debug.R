
cat("Using debug startup script\n")

future::plan(future::sequential)

# pool_size <- getOption('restbatch.max_concurrent_tasks', 1L)
# cat("Initializing pool according to `max_concurrent_tasks`:", pool_size, "\n")
#
#
# # if linux
# get_os <- function(){
#   if("windows" %in% .Platform$OS.type){
#     return("windows")
#   }
#   os <- stringr::str_to_lower(R.version$os)
#   if(stringr::str_detect(os, '^darwin')){
#     return('darwin')
#   }
#   if(stringr::str_detect(os, '^linux')){
#     return('linux')
#   }
#   if(stringr::str_detect(os, '^solaris')){
#     return('solaris')
#   }
#   if(stringr::str_detect(os, '^win')){
#     return('windows')
#   }
#   return('unknown')
# }
# if(get_os() == 'windows'){
#   future::plan(future::multisession, workers = pool_size + 1L)
# } else {
#   # so that outputs will be printed out
#   dipsaus::make_forked_clusters(workers = pool_size + 1L)
# }

