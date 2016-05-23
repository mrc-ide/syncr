## The most awkward thing here is doing the mocking up directory
## structures to mess with.
create_dirs <- function(root, x) {
  for (i in x) {
    dir.create(file.path(root, i), FALSE, TRUE)
  }
}

random_file <- function(path, max_len) {
  n <- sample(max_len)
  pool <- c(0:9, LETTERS, letters, " ", "\n")
  writeLines(paste(c(sample(pool, n, TRUE), "\n"), collapse=""), path)
}

## This might move into the package at some point
diff_bin <- function() {
  Sys_which("diff")
}

diff_dirs <- function(path1, path2) {
  system2(diff_bin(), c("-rq", path1, path2), stdout=FALSE, stderr=FALSE) == 0L
}
