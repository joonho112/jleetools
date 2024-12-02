#' Plot a Histogram with Optional Normal Distribution Overlay
#'
#' This function creates a histogram of the specified variable. Optionally, a normal
#' distribution curve can be overlaid, along with a vertical line indicating the mean.
#'
#' @param data A data frame containing the variable to be plotted.
#' @param var_name The name of the variable to plot (unquoted).
#' @param normal_dens Logical. If `TRUE`, overlays a normal density curve. Defaults to `TRUE`.
#'
#' @return A ggplot object.
#'
#' @details
#' - The histogram displays the density of the variable.
#' - If `normal_dens = TRUE`, a blue normal distribution curve overlays the histogram.
#' - A red dashed vertical line marks the mean of the variable.
#' - The plot uses the `theme_lucid()` from the **see** package for styling.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(see)
#' myhist(mtcars, mpg)
#' myhist(mtcars, mpg, normal_dens = FALSE)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_histogram stat_function geom_vline labs
#' @importFrom see theme_lucid
#' @importFrom rlang ensym as_string
#' @export
myhist <- function(data, var_name, normal_dens = TRUE) {
  # Ensure var_name is a symbol
  var_sym <- rlang::ensym(var_name)

  # Extract variable name as string for data indexing
  var_str <- rlang::as_string(var_sym)

  # Check if the variable exists in the dataset
  if (!var_str %in% names(data)) {
    stop(paste0("Variable '", var_str, "' not found in the dataset."))
  }

  # Calculate mean and standard deviation of the variable
  var_mean <- mean(data[[var_str]], na.rm = TRUE)
  var_sd <- sd(data[[var_str]], na.rm = TRUE)

  # Start building the plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = !!var_sym)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(density)),
      bins = 30, fill = "blue", alpha = 0.5
    ) +
    ggplot2::geom_vline(
      xintercept = var_mean,
      color = "red", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::labs(x = var_str, y = "Density") +
    see::theme_lucid()

  # Add normal density curve if requested
  if (normal_dens) {
    plot <- plot +
      ggplot2::stat_function(
        fun = dnorm,
        args = list(mean = var_mean, sd = var_sd),
        color = "blue", linewidth = 0.7
      )
  }

  return(plot)
}
