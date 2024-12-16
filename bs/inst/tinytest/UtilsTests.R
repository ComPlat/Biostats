library(tinytest)
library(bs)
library(readxl)

# Test 1: Valid input with an Excel file
test_file <- tempfile(fileext = ".xlsx")
write.csv(data.frame(a = 1:5, b = letters[1:5]), test_file, row.names = FALSE)
readxl::write_xlsx(read.csv(test_file), test_file)
result <- readData(test_file)
expect_equal(class(result), "data.frame")
expect_equal(nrow(result), 5)
expect_equal(ncol(result), 2)

# Test 2: Valid input with a CSV file (comma-separated)
test_that("readData reads a valid CSV file (comma-separated)", {
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = 1:5, b = letters[1:5]), test_file, row.names = FALSE)
  result <- readData(test_file)
  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 2)
})

# Test 3: File exceeds size limit
test_that("readData throws an error for file exceeding size limit", {
  test_file <- tempfile()
  write.csv(data.frame(a = 1:(50 * 1024^2 / 2)), test_file, row.names = FALSE)
  expect_error(readData(test_file), "File size exceeds the 50 MB limit.")
})

# Test 4: File with unknown separator
test_that("readData returns error for unknown separator", {
  test_file <- tempfile()
  writeLines("a|b|c\n1|2|3", test_file)
  result <- readData(test_file)
  expect_equal(result, "error")
})

# Test 5: File with semicolon separator
test_that("readData reads a file with semicolon separator", {
  test_file <- tempfile()
  writeLines("a;b;c\n1;2;3", test_file)
  result <- readData(test_file)
  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 3)
})

# Test 6: File with tab separator
test_that("readData reads a file with tab separator", {
  test_file <- tempfile()
  writeLines("a\tb\tc\n1\t2\t3", test_file)
  result <- readData(test_file)
  expect_equal(class(result), "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 3)
})

# Test 7: File with invalid path
test_that("readData throws an error for invalid path", {
  expect_error(readData("nonexistent_file.csv"), "cannot open the connection")
})

# Test 8: Data exceeds row or column limits
test_that("readData throws an error for data exceeding row or column limits", {
  test_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(matrix(1, nrow = 1e6 + 1, ncol = 2)), test_file, row.names = FALSE)
  expect_error(readData(test_file), "Data exceeds the limit of")

  write.csv(data.frame(matrix(1, nrow = 10, ncol = 1001)), test_file, row.names = FALSE)
  expect_error(readData(test_file), "Data exceeds the limit of")
})

# Test 9: Non-character input for path
test_that("readData throws an error for non-character path", {
  expect_error(readData(123), "is.character")
})
