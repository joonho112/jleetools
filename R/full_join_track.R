#' Perform a Full Join with Merge Diagnostics and Tracking
#'
#' `full_join_track` performs a full join on two data frames and provides merge diagnostics.
#' It can optionally add a tracking variable indicating the source of each row.
#'
#' @param x A data frame or tibble. The left data frame for the join.
#' @param y A data frame or tibble. The right data frame for the join.
#' @param by A character vector of variables to join by. If `NULL`, the default, joins by all variables with common names in both data frames.
#' @param suffix A character vector of length 2. Suffixes to be added to disambiguate overlapping column names.
#' @param .merge Logical. Should a merge tracking variable be added? Defaults to `FALSE`.
#' @param merge_var_name Character. The name of the merge tracking variable, if added. Defaults to `"merge_result"`.
#' @param ... Additional arguments passed to [dplyr::full_join()].
#'
#' @return A data frame resulting from the full join of `x` and `y`, with optional merge tracking.
#' @details
#' - The function prints diagnostics about the number of rows matched and unmatched from each data frame.
#' - If `.merge = TRUE`, a variable is added to indicate whether a row is from `x` only, `y` only, or matched.
#'
#' @examples
#' df1 <- tibble::tibble(id = 1:5, value1 = letters[1:5])
#' df2 <- tibble::tibble(id = 3:7, value2 = letters[3:7])
#'
#' # Perform a full join with diagnostics
#' full_join_track(df1, df2, by = "id")
#'
#' # Perform a full join and add a tracking variable
#' full_join_track(df1, df2, by = "id", .merge = TRUE)
#'
#' @importFrom dplyr full_join summarize mutate case_when select all_of
#' @importFrom rlang inform abort
#' @export

full_join_track <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                            .merge = FALSE, merge_var_name = "merge_result", ...) {
  # Capture input data frame names
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  # Ensure unique tracker variable names
  x_tracker <- make.unique(c(names(x), ".x_tracker"))[length(names(x)) + 1]
  y_tracker <- make.unique(c(names(y), ".y_tracker"))[length(names(y)) + 1]

  # Add tracker variables
  x[[x_tracker]] <- 1
  y[[y_tracker]] <- 1

  # Perform the join
  joined <- dplyr::full_join(x, y, by = by, suffix = suffix, ...)

  # Calculate merge diagnostics
  diagnostics <- dplyr::summarize(
    joined,
    matched = sum(!is.na(.data[[x_tracker]]) & !is.na(.data[[y_tracker]]), na.rm = TRUE),
    unmatched_x = sum(!is.na(.data[[x_tracker]]) & is.na(.data[[y_tracker]]), na.rm = TRUE),
    unmatched_y = sum(is.na(.data[[x_tracker]]) & !is.na(.data[[y_tracker]]), na.rm = TRUE)
  )

  # Print merge diagnostics with data frame names
  rlang::inform(paste0(
    diagnostics$unmatched_x, " rows ONLY from left data frame (", x_name, ")\n",
    diagnostics$unmatched_y, " rows ONLY from right data frame (", y_name, ")\n",
    diagnostics$matched, " rows matched"
  ))

  # Add merge_result variable if specified
  if (.merge) {
    if (merge_var_name %in% names(joined)) {
      rlang::abort(glue::glue("The variable '{merge_var_name}' already exists. Choose a different name."))
    }
    joined <- dplyr::mutate(
      joined,
      !!merge_var_name := dplyr::case_when(
        !is.na(.data[[x_tracker]]) & is.na(.data[[y_tracker]]) ~ paste0(x_name, "_only"),
        is.na(.data[[x_tracker]]) & !is.na(.data[[y_tracker]]) ~ paste0(y_name, "_only"),
        TRUE ~ "matched"
      )
    )
  }

  # Drop tracker variables
  joined <- dplyr::select(joined, -dplyr::all_of(c(x_tracker, y_tracker)))

  # Return the joined data
  return(joined)
}
