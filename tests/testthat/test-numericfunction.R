test_that("Compounded interest amount for positive integers", {
  expect_equal(comp_int(1000, 5, 2), 102.5)
})

test_that("Compounded interest amount when values are zero", {
  expect_error(comp_int(1000, 5, 0))
})

test_that("Compounded interest amount when values are negative", {
  expect_error(comp_int(-2000, 5, 2))
})

test_that("Compounded interest amount when values are missing", {
  expect_error(comp_int(-2000, NA, 2))
})

test_that("Compounded interest amount when values are not numeric", {
  expect_error(comp_int("a","b","c"))
})
