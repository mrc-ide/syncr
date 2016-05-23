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

is_absolute_path <- function(path) {
  grepl("^(/|[A-Z][a-z]:)", path)
}

is_relative_path <- function(path) {
  !is_absolute_path(path)
}
