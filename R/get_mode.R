#' Compute the mode (most frequent value)
#'
#' Calculates the statistical mode—the value(s) that occur most frequently—of an
#' atomic vector.
#'
#' @param x An atomic vector (numeric, character, factor, etc.). Lists and data
#'   frames are not supported.
#' @param tie_method How to handle ties when multiple values share the highest
#'   frequency. One of \code{"first"} (default) to return only the first
#'   encountered mode, or \code{"all"} to return every mode as a vector.
#' @param na_rm Logical. Should \code{NA} values be removed before computation?
#'   Defaults to \code{TRUE}.
#'
#' @return A vector containing the mode value(s). Returns \code{NA} if
#'   \code{x} contains no non-\code{NA} values.
#'
#' @examples
#' get_mode(c(1, 2, 2, 3))            # -> 2
#' get_mode(c("a", "b", "b", "c", "c"), tie_method = "all") # -> c("b", "c")
#' get_mode(c(NA, 1, 1, NA, 2), na_rm = TRUE)
#'
#' @export
get_mode <- function(x, tie_method = c("first", "all"), na_rm = TRUE) {
  tie_method <- match.arg(tie_method)

  # ------------------------------
  # Input validation
  # ------------------------------
  if (!is.atomic(x) || is.list(x)) {
    stop("`x` must be an atomic (non-list) vector.", call. = FALSE)
  }

  if (!is.logical(na_rm) || length(na_rm) != 1L) {
    stop("`na_rm` must be a single logical value.", call. = FALSE)
  }

  # Return early for zero-length vector
  if (length(x) == 0L) {
    return(NA)
  }

  # ------------------------------
  # Remove NA values if requested
  # ------------------------------
  if (na_rm) {
    x <- x[!is.na(x)]
  }

  # All elements were NA
  if (length(x) == 0L) {
    return(NA)
  }

  # ------------------------------
  # Compute frequencies
  # ------------------------------
  ux <- unique(x)
  freq <- tabulate(match(x, ux))
  modes <- ux[freq == max(freq)]

  # ------------------------------
  # Tie handling
  # ------------------------------
  if (tie_method == "first") {
    modes <- modes[1]
  }

  modes
}
