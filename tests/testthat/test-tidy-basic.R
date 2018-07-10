context("test-tidy-basic")
test_that("multiplication works", {
  expect_message(
    test_collection("basic", transformer = tidy_text)
  )
})
