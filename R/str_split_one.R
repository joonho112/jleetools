#' Split String into Components
#'
#' A simple wrapper around `stringr::str_split` to return the first element of the split result.
#' Useful when only a single split result is needed without the outer list structure.
#'
#' @param string A character string to be split.
#' @param pattern A character string or regular expression that serves as the delimiter for splitting `string`.
#' @param n An integer specifying the maximum number of splits. Default is `Inf` (no limit on splits).
#'
#' @return A character vector containing the components of the string split by `pattern`. If the input string
#' results in no splits, an empty string (`""`) is returned.
#'
#' @export
#'
#' @examples
#' # Split a string by a space
#' str_split_one("Hello World", pattern = " ")
#'
#' # Split a file path by slashes
#' str_split_one("/usr/local/bin", pattern = "/")
#'
#' # Split a CSV string
#' str_split_one("a,b,c", pattern = ",")
#'
#' # Handle an empty string
#' str_split_one("", pattern = ",")
#'
#' # Limit the number of splits
#' str_split_one("a,b,c", pattern = ",", n = 2)
str_split_one <- function(string, pattern, n = Inf) {

  result <- stringr::str_split(
    string = string,
    pattern = pattern,
    n = n)[[1]]

  if (length(result) == 0) {
    return("")
  }
  result
}
