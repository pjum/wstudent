context("ratio")

test_that("throw error for non-dataframe input", {
  
  expect_error(ratio(d))
  expect_error(ratio(1))

})