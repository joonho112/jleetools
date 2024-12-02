#' Generate a Correlation Plot
#'
#' Creates a correlation plot using the **corrr** package. The function calculates
#' correlations between specified variables, shaves the upper triangle of the matrix,
#' and generates a plot.
#'
#' @param df A data frame or tibble containing the variables to be analyzed.
#' @param ... Unquoted variable names to include in the correlation plot.
#'
#' @return A ggplot object representing the correlation matrix.
#'
#' @details
#' - This function uses the **corrr** package for calculating correlations and generating plots.
#' - The upper triangle of the correlation matrix is removed for readability.
#' - The resulting plot includes the correlation coefficients.
#' - Axis labels are rotated for better readability.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' library(corrr)
#'
#' # Example with the mtcars dataset
#' mycorplot1(mtcars, mpg, hp, wt, qsec)
#' }
#'
#' @importFrom dplyr select
#' @importFrom rlang quos
#' @importFrom corrr correlate shave rplot
#' @importFrom ggplot2 theme element_text
#' @export
mycorplot1 <- function(df, ...) {
  # Select specified variables from the dataset
  df_selected <- dplyr::select(df, !!!rlang::quos(...))

  # Calculate correlation, shave the upper triangle, and plot
  p <- df_selected %>%
    corrr::correlate() %>%
    corrr::shave() %>%
    corrr::rplot(print_cor = TRUE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))

  return(p)
}
