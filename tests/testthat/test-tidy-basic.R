context("test-tidy-basic")
test_that("Ordinary rmd document with header works", {
  expect_error(
    test_collection("basic", transformer = tidy_text),
    NA
  )
})


test_that("Bullet list", {
  expect_error(
    test_collection("bullet", transformer = tidy_text),
    NA
  )
})

test_that("long lines", {
  expect_error(
    test_collection("long-lines", transformer = tidy_text),
    NA
  )
})

test_that("width argument ", {
  expect_error(
    test_collection("width-argument", transformer = tidy_text, width = 10),
    NA
  )
})

test_that("enumeration", {
  expect_error(
    test_collection("enumeration", transformer = tidy_text),
    NA
  )
})
