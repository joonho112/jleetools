#' Tabulate Frequencies & Percentages (numeric-safe, full print)
#'
#' Creates a one-way frequency table like Stata’s `tab`, but
#' *keeps numeric values in numeric order* and lets you round the displayed
#' values of a continuous variable.
#'
#' @param data   A data frame.
#' @param var    Variable to tabulate (unquoted).
#' @param digits Integer; decimals in the **percent** column (default `1`).
#' @param value_digits NULL (default) or integer; if the variable is *numeric*
#'   and `value_digits` is set, the values are **rounded** to that many decimal
#'   places *before* tabulation and shown with trailing zeros removed.
#'
#' @return A **data.frame** with columns
#'   * `<var>` – distinct values (character, after optional rounding)
#'   * `n` – frequency
#'   * `percent` – formatted percentage string.
#'   A “Total” row (and a `<NA>` row, if applicable) is appended last.
#'
#' @examples
#' \dontrun{
#' tabb(mtcars, cyl)                 # categorical
#' tabb(mtcars, mpg, value_digits=2) # continuous, round to 2 dp
#' }
#'
#' @export
#' @importFrom dplyr count mutate arrange bind_rows
#' @importFrom rlang ensym as_label .data
#' @importFrom tibble tibble
tabb <- function(data, var, digits = 1, value_digits = 1) {
  stopifnot(is.data.frame(data))
  var_sym  <- rlang::ensym(var)
  var_name <- rlang::as_label(var_sym)

  # --- Optionally round numeric values BEFORE counting -----------------------
  if (is.numeric(data[[var_name]]) && !is.null(value_digits)) {
    data[[var_name]] <- round(data[[var_name]], value_digits)
  }

  # --- Raw counts ------------------------------------------------------------
  tab <- dplyr::count(data, !!var_sym, name = "n", .drop = FALSE)
  total_n <- sum(tab$n, na.rm = TRUE)

  # --- Percent (numeric) -----------------------------------------------------
  tab <- dplyr::mutate(tab, percent = n / total_n * 100)

  # --- Order: numeric vs character ------------------------------------------
  if (is.numeric(tab[[var_name]])) {
    tab <- dplyr::arrange(tab, !!var_sym)
  } else {
    tab <- dplyr::arrange(tab, as.character(!!var_sym))
  }

  # --- Format columns --------------------------------------------------------
  pct_fmt <- paste0('%.', digits, 'f%%')
  tab <- dplyr::mutate(
    tab,
    !!var_name := if (is.numeric(!!var_sym))
      formatC(!!var_sym, format = "f",
              digits = if (is.null(value_digits)) 6 else value_digits,
              drop0trailing = TRUE)
    else as.character(!!var_sym),
    percent = sprintf(pct_fmt, percent)
  )

  # --- Move NA row (if present) to bottom ------------------------------------
  if (any(is.na(tab[[var_name]]))) {
    na_row <- dplyr::filter(tab, is.na(.data[[var_name]]))
    tab    <- dplyr::filter(tab, !is.na(.data[[var_name]]))
    tab    <- dplyr::bind_rows(tab, na_row)
  }

  # --- Append Total row ------------------------------------------------------
  total_row <- tibble::tibble(
    !!var_name := "Total",
    n          = total_n,
    percent    = sprintf(pct_fmt, 100)
  )
  tab <- dplyr::bind_rows(tab, total_row)

  # convert tibble → data.frame for full printing
  as.data.frame(tab, stringsAsFactors = FALSE)
}


