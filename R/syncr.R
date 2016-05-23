##' Wrapper around rsync, with only a few options exposed.
##' \emph{WARNING}: This command modifies files on your computer and
##' therefore is potentially dangerous.  Files in \code{dest} may be
##' overwritten or deleted!
##'
##' This is a simple wrapper around a few of the most common arguments
##' to \code{rsync}.  For a more complete interface, see
##' \code{\link{rsync}}.
##' 
##' @title syncr
##' @param src Source files to copy.  See details
##' @param dest A single destination path, possibly remote.
##' @param archive Copy in archive mode, being recursive, preserving
##'   attributes etc?  Generally this is wanted.
##' @param compress Compress files on transfer?
##' @param verbose Be verbose?
##' @param delete Delete files on the remote machine.  When mirroring
##'   this is generally what you want to do but it is not enabled by
##'   default because it's potentially dangerous.
##' @param dry_run Don't actually do anything, but print what would be
##'   done instead.
##' @param drop_src_directory In the case where \code{src} is a single
##'   directory, don't copy the directory, but copy the contents.
##' @param args_only Don't run anything and instead return the
##'   arguments that would have been passed to rsync.
##' @export
syncr <- function(src, dest,
                  archive=TRUE, compress=TRUE, verbose=FALSE,
                  delete=FALSE, dry_run=FALSE,
                  drop_src_directory=FALSE, args_only=FALSE) {
  src <- fix_paths(src)
  dest <- fix_paths(dest)
  if (length(dest) != 1) {
    stop("Expected a single path for ")
  }

  if (drop_src_directory) {
    if (length(src) != 1L) {
      stop("Can only use drop_src_directory with a single path")
    } else if (is_remote_path(src) || is_directory(src)) {
      src <- paste0(src, "/")
    } else {
      stop("src must be a directory or remote name to use drop_src_directory")
    }
  }

  args <- c(character(0),
            if (archive) "--archive",
            if (compress) "--compress",
            if (verbose) "--verbose",
            if (delete) "--delete",
            if (dry_run) "--dry_run",
            src,
            dest)

  run_rsync(args, args_only)
}

is_directory <- function(path) {
  file.exists(path) & file.info(path, extra_cols=FALSE)[["isdir"]]
}

is_remote_path <- function(path) {
  grepl(":", path, fixed=TRUE)
}

fix_windows_paths <- function(x) {
  x <- gsub("\\", "/", x, fixed=TRUE) # no backslashes
  sub("^([A-Za-z]):", "/cygdrive/\\1", x) # drive translation
}

drop_trailing_slashes <- function(x) {
  sub("([^/])/+$", "\\1", x) # no trailing slashes
}

fix_paths <- function(x) {
  drop_trailing_slashes(fix_windows_paths(x))
}
