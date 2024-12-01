#' Split String into Components
#'
#' A simple wrapper around `strsplit` to return the first element of the split result.
#' Useful when only a single split result is needed without the outer list structure.
#'
#' @param x A character string to be split.
#' @param split A character string or regular expression that serves as the delimiter for splitting `x`.
#'
#' @return A character vector containing the components of the string split by `split`.
#'
#' @export
#'
#' @examples
#' # Split a string by a space
#' strsplit1("Hello World", split = " ")
#'
#' # Split a file path by slashes
#' strsplit1("/usr/local/bin", split = "/")
#'
#' # Split a CSV string
#' strsplit1("a,b,c", split = ",")
strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}

