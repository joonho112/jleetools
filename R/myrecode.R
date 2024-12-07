#' Recode Values in a Data Frame
#'
#' A simple wrapper function to recode specific values in a data frame column.
#'
#' @param data A data frame or tibble where the recoding should occur.
#' @param variable The name of the column (unquoted) where values should be recoded.
#' @param value_from The value to be replaced.
#' @param value_to The value to replace with.
#'
#' @return A data frame or tibble with the recoded values.
#'
#' @details
#' This function modifies the specified column in the data frame by replacing
#' occurrences of `value_from` with `value_to`. Other values in the column remain
#' unchanged. It uses `dplyr::mutate()` and `dplyr::case_when()` for efficient
#' recoding. The column to modify is passed as an unquoted variable name.
#'
#' @examples
#' library(dplyr)
#'
#' # Example data
#' df <- tibble::tibble(
#'   category = c("A", "B", "C", "B")
#' )
#'
#' # Recode values
#' myrecode(
#'   data = df,
#'   variable = category,
#'   value_from = "B",
#'   value_to = "Z"
#' )
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang ensym
#' @export
myrecode <- function(data, variable, value_from, value_to) {
  # Ensure the variable is a symbol
  variable <- rlang::ensym(variable)

  # Recode the specified value
  data_recode <- data %>%
    dplyr::mutate(
      !!variable := dplyr::case_when(
        # Replace value_from with value_to
        !!variable == value_from ~ value_to,

        # Keep other values as is
        TRUE ~ !!variable
      )
    )

  return(data_recode)
}
