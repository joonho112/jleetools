test_that("str_split_one() works as expected", {
  # Test basic functionality
  result <- str_split_one("a,b,c", pattern = ",")
  expect_equal(result, c("a", "b", "c"))

  # Test with a different delimiter
  result <- str_split_one("x;y;z", pattern = ";")
  expect_equal(result, c("x", "y", "z"))

  # Test with single character string
  result <- str_split_one("hello", pattern = ",")
  expect_equal(result, "hello")

  # Test with empty string
  result <- str_split_one("", pattern = ",")
  expect_equal(result, "")

  # Test edge case with no delimiter in string
  result <- str_split_one("abc", pattern = ",")
  expect_equal(result, "abc")

  # Test limiting the number of splits
  result <- str_split_one("a,b,c,d", pattern = ",", n = 2)
  expect_equal(result, c("a", "b,c,d"))

  # Test with no match for the delimiter
  result <- str_split_one("no delimiters here", pattern = ";")
  expect_equal(result, "no delimiters here")

  # Test with a complex regular expression
  result <- str_split_one("2023-11-16", pattern = "-")
  expect_equal(result, c("2023", "11", "16"))
})
