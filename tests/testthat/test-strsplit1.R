test_that("strsplit1() works as expected", {
  # Test basic functionality
  result <- strsplit1("a,b,c", split = ",")
  expect_equal(result, c("a", "b", "c"))

  # Test with a different delimiter
  result <- strsplit1("x;y;z", split = ";")
  expect_equal(result, c("x", "y", "z"))

  # Test with single character string
  result <- strsplit1("hello", split = ",")
  expect_equal(result, "hello")

  # Test with empty string
  result <- strsplit1("", split = ",")
  expect_equal(result, "")

  # Test edge case with no delimiter in string
  result <- strsplit1("abc", split = ",")
  expect_equal(result, "abc")
})
