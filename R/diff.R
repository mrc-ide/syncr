diff_bin <- function() {
  Sys_which("diff")
}

diff_dirs <- function(path1, path2) {
  system2(diff_bin(), c("-rq", path1, path2), stdout=FALSE, stderr=FALSE) == 0L
}
