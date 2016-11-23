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
##' @param verbose Be verbose? (Always \code{TRUE} when using \code{dry_run}).
##' @param delete Delete files on the remote machine.  When mirroring
##'   this is generally what you want to do but it is not enabled by
##'   default because it's potentially dangerous.
##' @param dry_run Don't actually do anything, but print what would be
##'   done instead.
##' @param relative Copy relative paths only, rather than the last
##'   part of the file name.  So if you have a directory \code{src}
##'   and you want to syncronise all the \code{.c} files in it, you
##'   could use \code{src="src/*.c", relative=TRUE}.  You can include
##'   a dot at any point in a path name to indicate where the relative
##'   directory should start from;
##'   e.g. \code{/absolute/path/to/./src/*.c} which will create a
##'   directory \code{src} with .c files in it.
##' @param drop_src_directory In the case where \code{src} is a single
##'   directory, don't copy the directory, but copy the contents.
##'
##' @param inplace Copy files in place.  This helps when copying to
##'   windows network shares where the unix emulation layer and
##'   windows don't get on very well.  The downside is that if the
##'   copy fails it leaves files in an inconsistent state.  By
##'   default, it will be used on windows and when the destination is
##'   an absolute path.
##'
##' @param args_only Don't run anything and instead return the
##'   arguments that would have been passed to rsync.
##' @export
syncr <- function(src, dest,
                  archive = TRUE, compress = TRUE, verbose = FALSE,
                  relative = FALSE, delete = FALSE, dry_run = FALSE,
                  drop_src_directory=FALSE, inplace = NULL,
                  args_only = FALSE) {
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

  if (dry_run) {
    verbose <- FALSE
  }

  if (is.null(inplace)) {
    inplace <- is_windows() && grepl("^/cygdrive", dest)
  }

  args <- c(character(0),
            if (archive) "--archive",
            if (compress) "--compress",
            if (verbose) "--verbose",
            if (relative) "--relative",
            if (delete) "--delete",
            if (dry_run) "--dry-run",
            if (inplace) "--inplace",
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
