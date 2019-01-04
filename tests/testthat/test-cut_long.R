context("test-cut_long")

test_that("width has length > 1", {
  expect_known_value(
    tidy_lines(lorem_ipsum(), width = c(20, 50, 31)),
    test_path("reference-objects/cut_long-1")
  )
})
