Sys_which <- function(names, error=TRUE) {
  path <- Sys.which(names)
  ok <- nzchar(path)
  if (all(ok)) {
    path
  } else if (error) {
    stop(paste(names[!ok], collapse=", "), " not found")
  } else {
    NULL
  }
}
