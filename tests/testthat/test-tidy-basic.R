context("test-tidy-basic")
test_that("Ordinary rmd document with header works", {
  expect_message(
    test_collection("basic", transformer = tidy_text)
  )
})


test_that("Bullet list", {
  expect_message(
    test_collection("bullet", transformer = tidy_text)
  )
})
