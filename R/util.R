Sys_which <- function(names) {
  path <- Sys.which(names)
  ok <- nzchar(path)
  if (all(ok)) {
    path
  } else {
    stop(paste(names[!ok], collapse=", "), " not found")
  }
}
