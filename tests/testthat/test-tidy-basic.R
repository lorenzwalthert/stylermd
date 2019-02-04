context("test-tidy-basic")
test_that("Ordinary rmd document with header works", {
  expect_warning(
    test_collection("basic", transformer = tidy_text),
    NA
  )
})


test_that("Bullet list", {
  expect_warning(
    test_collection("bullet", transformer = tidy_text),
    NA
  )
  expect_warning(
    test_collection("bullet", transformer = tidy_text),
    NA
  )
})

test_that("long lines", {
  expect_warning(
    test_collection("long-lines", transformer = tidy_text),
    NA
  )
})

test_that("width argument ", {
  expect_warning(
    test_collection("width-argument", transformer = tidy_text, width = 10),
    NA
  )
})

test_that("enumeration", {
  expect_warning(
    test_collection("enumeration", transformer = tidy_text),
    NA
  )
})

test_that("code chunks without preceding blank", {
  expect_warning(
    test_collection("code", "chunk-without-blank",
      transformer = tidy_text
    ),
    NA
  )
})

test_that("code", {
  expect_warning(
    test_collection("code", "linebreaks", transformer = tidy_text),
    NA
  )
})

test_that("code", {
  expect_warning(
    test_collection("code", "agnostic", transformer = tidy_text),
    NA
  )
})

test_that("code with latex $$ $$ is recognized", {
  expect_warning(
    test_collection("code", "latex-formula", transformer = tidy_text),
    NA
  )
})


test_that("don't break title", {
  expect_warning(
    test_collection("title", "dont-break", transformer = tidy_text),
    NA
  )
})
