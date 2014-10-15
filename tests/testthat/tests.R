library(censusR)


test_that("get_data() breaks with bad URL", {
  expect_error(get_data(base_url = "xxx.com"))
})

