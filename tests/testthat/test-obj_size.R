test_that("obj_size() works as expected", {

  # Test with a simple numeric vector
  vec <- 1:10
  expect_equal(obj_size(vec, format = "bytes", include_units = FALSE), as.numeric(object.size(vec)))
  expect_match(obj_size(vec, format = "bytes"), "bytes")
  expect_match(obj_size(vec, format = "KB"), "KB")
  expect_match(obj_size(vec, format = "MB"), "MB")
  expect_match(obj_size(vec, format = "GB"), "GB")

  # Test with a data frame
  df <- data.frame(x = 1:100, y = letters[1:100])
  size_in_kb <- as.numeric(object.size(df)) / 1024
  expect_equal(obj_size(df, format = "KB", include_units = FALSE), size_in_kb)

  # Test rounding and unit inclusion
  result <- obj_size(df, format = "KB")
  expect_match(result, "KB")
  expect_true(is.character(result))

  # Test invalid format handling (suppress warning during test)
  result_invalid <- suppressWarnings(obj_size(df, format = "invalid_format"))
  expect_match(result_invalid, "bytes")

  # Test with include_units = FALSE
  result_no_units <- obj_size(df, format = "MB", include_units = FALSE)
  expect_true(is.numeric(result_no_units))

  # Test with empty object
  empty_list <- list()
  empty_size <- as.numeric(object.size(empty_list))  # Adjust to expected value
  expect_equal(obj_size(empty_list, format = "bytes", include_units = FALSE), empty_size)

  # Test with NULL
  null_size <- as.numeric(object.size(NULL))
  expect_equal(obj_size(NULL, format = "bytes", include_units = FALSE), null_size)

  # Test with large object
  large_vec <- rep(1, 1e6)
  size_in_mb <- as.numeric(object.size(large_vec)) / 1024^2
  expect_equal(obj_size(large_vec, format = "MB", include_units = FALSE), size_in_mb)
})
