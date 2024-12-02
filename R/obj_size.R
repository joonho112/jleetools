#' Measure Object Size
#'
#' Computes the size of an R object in bytes, kilobytes, megabytes, or gigabytes.
#'
#' @param x An R object whose size is to be measured.
#' @param format A character string specifying the size format.
#'        Options are `"bytes"`, `"KB"`, `"MB"`, `"GB"`. Defaults to `"MB"`.
#' @param include_units Logical. If `TRUE`, appends the size units (e.g., MB, KB) to the output. Defaults to `TRUE`.
#'
#' @return A numeric value or a character string representing the size of the object in the specified format.
#'         If `include_units = TRUE`, the result is a character string with units. If `FALSE`, only the numeric size is returned.
#'
#' @details The function uses base R's `object.size()` to compute the memory size of an object and converts it to the desired unit.
#'          Units can be specified as `"bytes"`, `"KB"`, `"MB"`, or `"GB"`. The size is rounded to three decimal places for readability.
#'
#' @examples
#' # Default usage (size in MB with units)
#' obj_size(mtcars)
#'
#' # Size in kilobytes
#' obj_size(mtcars, format = "KB")
#'
#' # Size in bytes
#' obj_size(mtcars, format = "bytes")
#'
#' # Size in gigabytes without units
#' obj_size(mtcars, format = "GB", include_units = FALSE)
#'
#' # Invalid format defaults to bytes with a warning
#' obj_size(mtcars, format = "invalid_format")
#'
#' @importFrom utils object.size
#' @export
obj_size <- function(x, format = "MB", include_units = TRUE) {
  # Conversion factors
  conversion_factors <- c(
    "bytes" = 1,
    "KB" = 1024,
    "MB" = 1024^2,
    "GB" = 1024^3
  )

  # Validate format
  if (!format %in% names(conversion_factors)) {
    warning("Invalid format. Defaulting to 'bytes'.")
    format <- "bytes"
  }

  # Compute size
  size_in_bytes <- as.numeric(utils::object.size(x))
  size_converted <- size_in_bytes / conversion_factors[[format]]

  # Add units if requested
  if (include_units) {
    paste0(format(round(size_converted, 3), big.mark = ","), " ", format)
  } else {
    size_converted
  }
}
