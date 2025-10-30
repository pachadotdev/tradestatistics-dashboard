test_that("rv and rvtl work", {
  expect_true(
    inherits(rv, "function")
  )
  expect_true(
    inherits(rvtl, "function")
  )
})
