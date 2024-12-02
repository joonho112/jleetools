#' Tabulate a Variable with Percentages
#'
#' The `tabb` function provides a simple way to tabulate a variable, calculate frequencies, and format percentages.
#' It is particularly useful for creating a quick summary of categorical variables, inspired by Stata's `tab` command.
#'
#' @param data A data frame containing the variable to be tabulated.
#' @param var The variable to tabulate (unquoted).
#'
#' @return A tibble containing the unique values of the variable, their frequencies, and percentages.
#' A "Total" row is also included with the overall sum.
#'
#' @examples
#' \dontrun{
#' # Example with the mtcars dataset
#' tabb(mtcars, cyl)
#'
#' # Example with a custom dataset
#' df <- data.frame(category = c("A", "B", "A", "C", "B", "A"))
#' tabb(df, category)
#' }
#'
#' @importFrom dplyr select arrange mutate
#' @importFrom janitor tabyl adorn_totals adorn_pct_formatting
#' @importFrom rlang enquo as_label
#' @importFrom magrittr %>%
#' @export
tabb <- function(data, var) {
  # Capture the variable name
  var <- rlang::enquo(var)
  var_name <- rlang::as_label(var)

  # Check if the dataset is empty
  if (nrow(data) == 0) {
    message("The dataset is empty. Returning an empty tibble.")
    return(tibble::tibble(
      !!var_name := character(0),
      n = integer(0),
      percent = character(0)
    ))
  }

  # Check if the variable exists in the dataset
  if (!var_name %in% colnames(data)) {
    stop(glue::glue("Variable `{var_name}` not found in the dataset."))
  }

  # Perform tabulation
  result <- data %>%
    janitor::tabyl(!!var) %>%
    janitor::adorn_totals() %>%
    janitor::adorn_pct_formatting() %>%
    dplyr::mutate(
      percent = dplyr::case_when(
        .data[[var_name]] == "Total" ~ "100.0%",  # Explicitly handle the "Total" row
        TRUE ~ percent
      )
    ) %>%
    dplyr::select(all_of(c(var_name, "n", "percent"))) %>%
    dplyr::arrange(.data[[var_name]])

  return(result)
}
