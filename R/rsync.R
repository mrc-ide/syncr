##' Wrapper around all of \emph{rsync}'s options.  I'm not sure how
##' useful this will be (because you'll really need to read rsync's
##' extensive manual to use this, but perhaps it will be useful.
##'
##' @title Wrapper around rsync
##' @param verbose increase verbosity
##' @param quiet suppress non-error messages
##' @param no_motd suppress daemon-mode MOTD (see caveat)
##' @param checksum skip based on checksum, not mod-time & size
##' @param archive archive mode; same as recursive, links, perms, times, group, owner, devices specials but no hard_links
##' @param recursive recurse into directories
##' @param relative use relative path names
##' @param no_implied_dirs don't send implied dirs with relative
##' @param backup make backups (see suffix & backup_dir)
##' @param backup_dir make backups into hierarchy based in DIR
##' @param suffix backup suffix (default ~ w/o backup_dir)
##' @param update skip files that are newer on the receiver
##' @param inplace update destination files in-place
##' @param append append data onto shorter files
##' @param dirs transfer directories without recursing
##' @param links copy symlinks as symlinks
##' @param copy_links transform symlink into referent file/dir
##' @param copy_unsafe_links only "unsafe" symlinks are transformed
##' @param safe_links ignore symlinks that point outside the tree
##' @param copy_dirlinks transform symlink to dir into referent dir
##' @param keep_dirlinks treat symlinked dir on receiver as dir
##' @param hard_links preserve hard links
##' @param perms preserve permissions
##' @param executability preserve executability
##' @param chmod affect file and/or directory permissions
##' @param owner preserve owner (super-user only)
##' @param group preserve group
##' @param devices preserve device files (super-user only)
##' @param specials preserve special files
##' @param times preserve times
##' @param omit_dir_times omit directories when preserving times
##' @param super receiver attempts super-user activities
##' @param sparse handle sparse files efficiently
##' @param dry_run show what would have been transferred
##' @param whole_file copy files whole (without rsync algorithm)
##' @param one_file_system don't cross filesystem boundaries
##' @param block_size force a fixed checksum block-size
##' @param rsh specify the remote shell to use
##' @param rsync_path specify the rsync to run on remote machine
##' @param existing skip creating new files on receiver
##' @param ignore_existing skip updating files that exist on receiver
##' @param remove_source_files sender removes synchronized files (non-dir)
##' @param delete delete extraneous files from dest dirs
##' @param delete_before receiver deletes before transfer (default)
##' @param delete_during receiver deletes during xfer, not before
##' @param delete_after receiver deletes after transfer, not before
##' @param delete_excluded also delete excluded files from dest dirs
##' @param ignore_errors delete even if there are I/O errors
##' @param force force deletion of dirs even if not empty
##' @param max_delete don't delete more than NUM files
##' @param max_size don't transfer any file larger than SIZE
##' @param min_size don't transfer any file smaller than SIZE
##' @param partial keep partially transferred files
##' @param partial_dir put a partially transferred file into DIR
##' @param delay_updates put all updated files into place at end
##' @param prune_empty_dirs prune empty directory chains from file_list
##' @param numeric_ids don't map uid/gid values by user/group name
##' @param timeout set I/O timeout in seconds
##' @param ignore_times don't skip files that match size and time
##' @param size_only skip files that match in size
##' @param modify_window compare mod_times with reduced accuracy
##' @param temp_dir create temporary files in directory DIR
##' @param fuzzy find similar file for basis if no dest file
##' @param compare_dest also compare received files relative to DIR
##' @param copy_dest ... and include copies of unchanged files
##' @param link_dest hardlink to files in DIR when unchanged
##' @param compress compress file data during the transfer
##' @param compress_level explicitly set compression level
##' @param cvs_exclude auto_ignore files in the same way CVS does
##' @param filter add a file_filtering RULE
##' @param exclude exclude files matching PATTERN
##' @param exclude_from read exclude patterns from FILE
##' @param include don't exclude files matching PATTERN
##' @param include_from read include patterns from FILE
##' @param files_from read list of source-file names from FILE
##' @param from0 all *from/filter files are delimited by 0s
##' @param address bind address for outgoing socket to daemon
##' @param port specify double_colon alternate port number
##' @param sockopts specify custom TCP options
##' @param blocking_io use blocking I/O for the remote shell
##' @param stats give some file-transfer stats
##' @param eight_bit_output leave high-bit chars unescaped in output
##' @param human_readable output numbers in a human-readable format
##' @param progress show progress during transfer
##' @param itemize_changes output a change-summary for all updates
##' @param out_format output updates using the specified FORMAT
##' @param log_file log what we're doing to the specified FILE
##' @param log_file_format log updates using the specified FMT
##' @param password_file read password from FILE
##' @param list_only list the files instead of copying them
##' @param bwlimit limit I/O bandwidth; KBytes per second
##' @param write_batch write a batched update to FILE
##' @param only_write_batch like write_batch but w/o updating dest
##' @param read_batch read a batched update from FILE
##' @param protocol force an older protocol version to be used
##' @param checksum_seed set block/file checksum seed (advanced)
##' @param ipv4 prefer IPv4
##' @param ipv6 prefer IPv6
##' @param extended_attributes copy extended attributes, resource forks
##' @param cache disable fcntl(F_NOCACHE)
##' @inheritParams syncr
##' @export
rsync <- function(src, dest, verbose=FALSE, quiet=FALSE,
                  no_motd=FALSE, checksum=FALSE, archive=FALSE,
                  recursive=FALSE, relative=FALSE,
                  no_implied_dirs=FALSE, backup=FALSE,
                  backup_dir=NULL, suffix=NULL, update=FALSE,
                  inplace=FALSE, append=FALSE, dirs=FALSE,
                  links=FALSE, copy_links=FALSE,
                  copy_unsafe_links=FALSE, safe_links=FALSE,
                  copy_dirlinks=FALSE, keep_dirlinks=FALSE,
                  hard_links=FALSE, perms=FALSE, executability=FALSE,
                  chmod=NULL, owner=FALSE, group=FALSE, devices=FALSE,
                  specials=FALSE, times=FALSE, omit_dir_times=FALSE,
                  super=FALSE, sparse=FALSE, dry_run=FALSE,
                  whole_file=FALSE, one_file_system=FALSE,
                  block_size=NULL, rsh=NULL, rsync_path=NULL,
                  existing=FALSE, ignore_existing=FALSE,
                  remove_source_files=FALSE, delete=FALSE,
                  delete_before=FALSE, delete_during=FALSE,
                  delete_after=FALSE, delete_excluded=FALSE,
                  ignore_errors=FALSE, force=FALSE, max_delete=NULL,
                  max_size=NULL, min_size=NULL, partial=FALSE,
                  partial_dir=NULL, delay_updates=FALSE,
                  prune_empty_dirs=FALSE, numeric_ids=FALSE,
                  timeout=NULL, ignore_times=FALSE, size_only=FALSE,
                  modify_window=NULL, temp_dir=NULL, fuzzy=FALSE,
                  compare_dest=NULL, copy_dest=NULL, link_dest=NULL,
                  compress=FALSE, compress_level=NULL,
                  cvs_exclude=FALSE, filter=NULL, exclude=NULL,
                  exclude_from=NULL, include=NULL, include_from=NULL,
                  files_from=NULL, from0=FALSE, address=NULL,
                  port=NULL, sockopts=NULL, blocking_io=FALSE,
                  stats=FALSE, eight_bit_output=FALSE,
                  human_readable=FALSE, progress=FALSE,
                  itemize_changes=FALSE, out_format=NULL,
                  log_file=NULL, log_file_format=NULL,
                  password_file=NULL, list_only=FALSE, bwlimit=NULL,
                  write_batch=NULL, only_write_batch=NULL,
                  read_batch=NULL, protocol=NULL, checksum_seed=NULL,
                  ipv4=FALSE, ipv6=FALSE, extended_attributes=FALSE,
                  cache=FALSE, drop_src_directory=FALSE, args_only=FALSE) {
  src <- fix_paths(src)
  dest <- fix_paths(dest)
  if (length(dest) != 1) {
    stop("Expected a single path for ")
  }

  if (drop_src_directory) {
    if (length(src) != 1L) {
      stop("Can only use drop_src_directory with a single path")
    } else if (is_remote_path(src) || is_directory(src)) {
      if (grepl("[^/]$", src)) {
        src <- paste0(src, "/")
      }
    } else {
      stop("src must be a directory or remote name to use drop_src_directory")
    }
  }

  args <- list()

  as_arg <- function(x) {
    paste0("--", gsub("_", "-", x))
  }
  f_logical <- function(nm) {
    x <- as.vector(get(nm))
    if (isTRUE(x)) {
      as_arg(nm)
    } else if (identical(x, FALSE)) {
      NULL
    } else {
      stop("Invalid input for ", nm)
    }
  }
  f_value <- function(nm) {
    x <- as.vector(get(nm))
    if (length(x) == 1L) {
      c(as_arg(nm), as.character(x))
    } else if (is.null(x)) {
      NULL
    } else {
      stop("Invalid input for ", nm)
    }
  }

  args_logical <- unlist(lapply(rsync_logical_arg, f_logical))
  args_value <- unlist(lapply(rsync_value_arg, f_value))
  if ("eight_bit_output" %in% names(args_logical)) {
    names(args_logical)[names(args_logical) == "eight_bit_output"] <-
      "8_bit_output"
  }
  args <- c(args_logical, args_value, src, dest)

  run_rsync(args, args_only)
}


rsync_value_arg <- names(which(vapply(formals(rsync), is.null, logical(1))))
rsync_logical_arg <- setdiff(
  names(which(vapply(formals(rsync), identical, logical(1), FALSE))),
  c("drop_src_directory", "args_only"))
