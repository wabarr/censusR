library(censusR)
context("first test")

test_that("get_data() breaks with bad URL", {
  expect_equal(1, 1)
  expect_equal(2, 2.0)
  expect_error(get_data(base_url = "xxx"))
})