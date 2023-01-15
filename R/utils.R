# utility functions

today<-function(asStr=F, fmt="%Y%m%d") {
  return(format(Sys.Date(), fmt))
}

download_file<-function(url, ...) {
  ok<-utils::download.file(url, ...)
  if(ok > 0) {
    stop(glue::glue("Downloading failed: {url}"))
  } else {
    message(glue::glue("Downloading successful: {url}"))
  }
}
