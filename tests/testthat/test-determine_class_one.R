context("test-determine_class_one")

test_that("bullets", {
  expect_equal(
    determine_class_one("* A bullet"),
    list(class = "bullet", indent = 0)
  )

  expect_equal(
    determine_class_one("  * A bullet"),
    list(class = "bullet", indent = 2)
  )

  expect_equal(
    determine_class(c("- a bullet", "  - A bullet")),
    list(
      list(class = "bullet", indent = 0),
      list(class = "bullet", indent = 2)
    )
  )
})


test_that("enumeration", {
  expect_equal(
    determine_class_one("1. A bullet"),
    list(class = "enumeration", indent = 0)
  )

  expect_equal(
    determine_class_one("   1. A bullet"),
    list(class = "enumeration", indent = 3)
  )
})

