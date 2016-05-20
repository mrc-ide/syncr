context("syncr")

test_that("basic", {
  path1 <- tempfile()
  create_dirs(path1, c("a/a1", "b/b1", "c/c1"))

  path2 <- tempfile()
  dir.create(path2)
  expect_false(diff_dirs(path1, path2))

  syncr(path1, path2, drop_src_directory=TRUE)
  expect_equal(dir(path1), dir(path2))
  expect_true(diff_dirs(path1, path2))

  ## Now, delete a directory from path2 and watch it get recreated:
  unlink(file.path(path2, "b"), recursive=TRUE)
  expect_false(diff_dirs(path1, path2))

  ## And sync it back:
  syncr(path1, path2, drop_src_directory=TRUE)
  expect_true(diff_dirs(path1, path2))

  ## In contrast, delete a directory from the *source* and it won't
  ## end up disappearing by default:
  unlink(file.path(path1, "b"), recursive=TRUE)
  expect_false(diff_dirs(path1, path2))
  syncr(path1, path2, drop_src_directory=TRUE)
  expect_false(diff_dirs(path1, path2))
  expect_false(file.exists(file.path(path1, "b")))
  expect_true(file.exists(file.path(path2, "b")))

  ## Sync again with delete on:
  syncr(path1, path2, drop_src_directory=TRUE, delete=TRUE)
  expect_true(diff_dirs(path1, path2))
  expect_false(file.exists(file.path(path1, "b")))
  expect_false(file.exists(file.path(path2, "b")))
})

test_that("Group of files", {
  path1 <- tempfile()
  dir.create(path1, FALSE, TRUE)

  files_c <- replicate(5, tempfile(tmpdir=path1, fileext=".c"))
  for (f in files_c) {
    random_file(f, 100)
  }
  files_h <- replicate(5, tempfile(tmpdir=path1, fileext=".h"))
  for (f in files_h) {
    random_file(f, 100)
  }

  path2 <- tempfile()
  dir.create(path2, FALSE, TRUE)

  syncr(file.path(path1, "*.c"), path2)
  expect_false(diff_dirs(path1, path2))

  files_c2 <- file.path(path2, basename(files_c))
  expect_true(all(file.exists(files_c2)))
  expect_equal(unname(tools::md5sum(files_c2)),
               unname(tools::md5sum(files_c)))

  syncr(file.path(path1, "*.h"), path2)
  expect_true(diff_dirs(path1, path2))
})
