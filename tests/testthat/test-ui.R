context("test-ui")

test_that("tidying file", {
  tf <- tempfile()
  file.copy(testthat_file("ui/long-lines-3-backup"), tf)
  tidy_file(tf)
  expect_equal(
    readLines(tf),
    readLines(testthat_file("ui/long-lines-3-clean"))
  )
})
