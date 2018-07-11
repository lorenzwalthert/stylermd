context("test-ui")

test_that("tidying file", {
  tf <- tempfile()
  file.copy(testthat_file("ui/long-lines-3-backup"), tf)
  tidy_file(tf)
  expect_known_output(tf, testthat_file("ui/long-lines-3-clean"), update = FALSE)
})
