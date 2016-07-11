# syncr

> Synchronise Directories

Syncronise directories using `rsync`.  This just uses `system2` to shell out to call `rsync`; not any fancy library connections or anything.  On windows, install [Rtools](http://cran.r-project.org/bin/windows/Rtools) and this package will work.

**WARNING** This package just wraps `rsync`.  It doesn't protect you from it deleting all your files, overwriting your thesis or painting your pet dog.

## Installation

```r
devtools::install_github("richfitz/syncr")
```

## Usage

```r
syncr::rsync(src = system.file("DESCRIPTION", package = "syncr"), dest = tempfile(), dry_run = TRUE)
```
