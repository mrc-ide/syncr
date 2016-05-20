##' Wrapper around rsync, with only a few options exposed.
##' \emph{WARNING}: This command modifies files on your computer and
##' therefore is potentially dangerous.  Files in \code{dest} may be
##' overwritten or deleted!
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

  if (args_only) {
    return(args)
  }

  code <- system2(rsync_bin(), args)
  if (code == 0) {
    invisible(TRUE)
  } else {
    stop(sprintf("rsync failed with code %d: %s", code, rsync_error_type(code)))
  }
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

rsync_bin <- function() {
  Sys_which("rsync")
}

rsync_error_type <- function(code) {
  codes <- c("0"="Success",
             "1"="Syntax or usage error",
             "2"="Protocol incompatibility",
             "3"="Errors selecting input/output files, dirs",
             "4"="Requested action not supported",
             ## : an attempt was made to manipu- late 64-bit files on
             ## a platform that cannot support them; or an option was
             ## specified that is supported by the client and not by
             ## the server.
             "5"="Error starting client-server protocol",
             "6"="Daemon unable to append to log-file",
             "10"="Error in socket I/O",
             "11"="Error in file I/O",
             "12"="Error in rsync protocol data stream",
             "13"="Errors with program diagnostics",
             "14"="Error in IPC code",
             "20"="Received SIGUSR1 or SIGINT",
             "21"="Some error returned by waitpid()",
             "22"="Error allocating core memory buffers",
             "23"="Partial transfer due to error",
             "24"="Partial transfer due to vanished source files",
             "25"="The --max-delete limit stopped deletions",
             "30"="Timeout in data send/receive")
  code <- as.character(code)
  i <- match(code, names(codes))
  if (is.na(i)) {
    "Unknown error"
  } else {
    codes[[code]]
  }
}
