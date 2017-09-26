run_rsync <- function(args, args_only) {
  if (args_only) {
    args
  } else {
    code <- system2(rsync_bin(), args)
    if (code == 0) {
      invisible(TRUE)
    } else {
      stop(sprintf("rsync failed with code %d: %s",
                   code, rsync_error_type(code)))
    }
  }
}

rsync_bin <- function(error=TRUE) {
  getOption("syncr.rsync", unname(Sys_which("rsync", error)))
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


##' Check if there is an rsync we can use.
##' @title Check if there is an rsync we can use
##' @export
has_rsync <- function() {
  !is.null(rsync_bin(FALSE))
}

prepare_paths <- function(src, dest, drop_src_directory) {
  src_is_dir <- is_remote_path(src) || is_directory(src)
  src <- fix_paths(src)
  dest <- fix_paths(dest)
  if (length(dest) != 1) {
    stop("Expected a single path for ")
  }

  if (drop_src_directory) {
    if (length(src) != 1L) {
      stop("Can only use drop_src_directory with a single path")
    } else if (src_is_dir) {
      if (grepl("[^/]$", src)) {
        src <- paste0(src, "/")
      }
    } else {
      stop("src must be a directory or remote name to use drop_src_directory")
    }
  }

  list(src = src, dest = dest)
}
