load_yaml <- function(file, ..., map = NULL){
  re <- yaml::read_yaml(file = file, ...)
  if(!inherits(map, 'fastmap2')){
    map <- dipsaus::fastmap2()
  }
  for(nm in names(re)){
    if(nm != ''){
      map[[nm]] <- re[[nm]]
    }
  }
  map
}

save_yaml <- function(x, file, ...){
  yaml::write_yaml(as.list(x), file = file, ...)
  invisible(normalizePath(file))
}
