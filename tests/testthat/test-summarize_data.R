library(testthat)
library(simpleEnsembleGroup8)

# Test for correct calculation of mean and median
test_that("summarize_data calculates mean and median correctly", {
  test_vec <- c(1, 2, 3, 4, 5)
  result <- summarize_data(test_vec)

  expect_equal(result$mean, mean(test_vec))
  expect_equal(result$median, median(test_vec))
})

# Test for handling of NA values
test_that("summarize_data handles NA values correctly", {
  test_vec <- c(1, NA, 3, 4, 5)
  result <- summarize_data(test_vec)

  expect_equal(result$mean, mean(test_vec, na.rm = TRUE))
  expect_equal(result$median, median(test_vec, na.rm = TRUE))
})

# Test for numeric vector input requirement
test_that("summarize_data requires numeric vector input", {
  non_numeric_input <- "not a numeric vector"

  expect_error(summarize_data(non_numeric_input))
})
