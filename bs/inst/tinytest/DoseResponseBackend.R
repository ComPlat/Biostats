library(tinytest)

# Test cases for errorClass
test_errorClass <- function() {
  err <- bs:::errorClass$new("An error message")
  expect_false(err$isNull())
  expect_equal(err$error_message, "An error message")
  err_null <- bs:::errorClass$new()
  expect_true(err_null$isNull())
}

# Test cases for shapenumber
test_shapenumber <- function() {
  expect_equal(bs:::shapenumber(123.456), signif(123.456))
  expect_equal(bs:::shapenumber(Inf), NA)
  expect_equal(bs:::shapenumber(-Inf), NA)
  expect_equal(bs:::shapenumber(NA), NA)
}

# Test cases for robust_68_percentile
test_robust_68_percentile <- function() {
  residuals <- c(1, 2, 3, 4, 5)
  expect_true(is.double(bs:::robust_68_percentile(residuals)))

  # Test with Gaussian residuals
  residuals <- rnorm(100, mean = 0, sd = 1)
  p68 <- bs:::robust_68_percentile(residuals)
  expect_true(
    abs(p68 - quantile(abs(residuals), 0.6827)) < 0.1
  )

  # Test with outliers
  residuals_with_outliers <- c(rnorm(95, mean = 0, sd = 1), 10, 15, -20, 25)
  p68_robust <- bs:::robust_68_percentile(residuals_with_outliers)
  expect_true(p68_robust < sd(residuals_with_outliers)) # Robust measure

  # Test edge case: identical residuals
  identical_residuals <- rep(1, 10)
  p68_identical <- bs:::robust_68_percentile(identical_residuals)
  expect_true(abs(p68_identical - 1) < 0.01)
}

# Test cases for rsdr
test_rsdr <- function() {
  # Basic functionality
  residuals <- c(1, 2, 3, 4, 5)
  expect_true(is.double(bs:::rsdr(residuals, 1)))

  # Test with Gaussian residuals
  residuals_gaussian <- rnorm(100, mean = 0, sd = 1)
  rsdr_value <- bs:::rsdr(residuals_gaussian, 1)
  expect_true(rsdr_value > 0)

  # Test with outliers
  residuals_with_outliers <- c(rnorm(95, mean = 0, sd = 1), 10, 15, -20, 25)
  rsdr_outliers <- bs:::rsdr(residuals_with_outliers, 2)
  expect_true(rsdr_outliers < sd(residuals_with_outliers)) # Robust measure

  # Test small sample sizes
  small_residuals <- c(-2, -1, 0, 1, 2)
  rsdr_small <- bs:::rsdr(small_residuals, 1)
  expect_true(rsdr_small > 0)
}

# Test cases for false_discovery_rate
test_false_discovery_rate <- function() {
  residuals <- c(1, 2, 3, 4, 5)
  include <- bs:::false_discovery_rate(residuals)
  expect_true(is.logical(include))
  expect_equal(length(include), length(residuals))
}

# Test cases for ic50
test_ic50 <- function() {
  library(drc)
  data <- data.frame(
    abs = c(0.5, 0.6, 0.7, 0.8, 0.9),
    conc = c(1, 10, 100, 1000, 10000),
    names = c("A", "A", "A", "A", "A")
  )
  result <- bs:::ic50(data, "abs", "conc", "names", NULL, FALSE, FALSE)
  expect_true(is.list(result))
  expect_true(is.data.frame(result[[1]][[1]]))
}

# Run all tests
test_errorClass()
test_shapenumber()
test_robust_68_percentile()
test_rsdr()
test_false_discovery_rate()
test_ic50()
