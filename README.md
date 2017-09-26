# syncr

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/mrc-ide/syncr.svg?branch=master)](https://travis-ci.org/mrc-ide/syncr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mrc-ide/syncr?branch=master&svg=true)](https://ci.appveyor.com/project/mrc-ide/syncr)
[![codecov.io](https://codecov.io/github/mrc-ide/syncr/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/syncr?branch=master)

> Synchronise Directories

Syncronise directories using `rsync`.  This just uses `system2` to shell out to call `rsync`; not any fancy library connections or anything.  On windows, install [Rtools](http://cran.r-project.org/bin/windows/Rtools) and this package will work.

**WARNING** This package just wraps `rsync`.  It doesn't protect you from it deleting all your files, overwriting your thesis or painting your pet dog.

## Installation

```r
drat:::add("mrc-ide")
install.packages("syncr")
```

## Usage

```r
syncr::rsync(src = system.file("DESCRIPTION", package = "syncr"), dest = tempfile(), dry_run = TRUE)
```
