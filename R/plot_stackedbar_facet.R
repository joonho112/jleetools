#' Plot a Stacked Bar Chart with Faceting and Labels
#'
#' Generates a stacked bar chart with required faceting. The function also includes labels
#' for percentages within each bar and total group sums above each bar. The y-axis is
#' formatted with commas, and bar colors can be customized using a specified color palette.
#'
#' @param data A data frame containing the variables for plotting.
#' @param x The variable to use for the x-axis (unquoted).
#' @param y The variable to use for the y-axis (unquoted, numeric).
#' @param fill The variable to use for the fill aesthetic (unquoted, categorical).
#' @param facet The variable to use for faceting (unquoted, categorical).
#' @param palette_name A string specifying the color palette from `RColorBrewer`. Defaults to `"Paired"`.
#' @param decimal An integer specifying the number of decimal places to round the group sum labels. Defaults to `0`.
#'
#' @return A ggplot object representing the stacked bar chart with optional faceting and labels.
#'
#' @examples
#' # Example dataset
#' library(dplyr)
#' df <- data.frame(
#'   year = rep(2021:2023, each = 6),
#'   category = rep(c("A", "B", "C", "D", "E", "F"), 3),
#'   value = c(100, 200, 150, 300, 250, 100, 200, 300, 400, 250, 150, 100, 300, 100, 200, 150, 400, 300),
#'   group = rep(c("G1", "G2"), 9)
#' )
#'
#' # Generate a stacked bar chart
#' plot_stackedbar_facet(
#'   data = df,
#'   x = year,
#'   y = value,
#'   fill = category,
#'   facet = group,
#'   palette_name = "Set3",
#'   decimal = 1
#' )
#'
#' @importFrom dplyr mutate group_by ungroup summarize
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 ggplot aes geom_bar geom_text facet_grid scale_fill_manual scale_y_continuous
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales comma
#' @importFrom rlang ensym
#' @export
plot_stackedbar_facet <- function(data, x, y, fill, facet,
                                  palette_name = "Paired", decimal = 0) {
  # Ensure column names are symbols
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  fill <- rlang::ensym(fill)
  facet <- rlang::ensym(facet)

  # Process data to calculate percentages and labels
  data_processed <- data %>%
    dplyr::mutate(!!fill := forcats::fct_rev(factor(!!fill))) %>%
    dplyr::group_by(!!x, !!facet) %>%
    dplyr::mutate(
      group_sum = sum(!!y, na.rm = TRUE),
      percent = (!!y / group_sum) * 100,
      label_text = paste0(sprintf("%.1f", percent), "%")
    ) %>%
    dplyr::ungroup()

  # Create a summary dataframe for bar-end text labels
  data_sum <- data_processed %>%
    dplyr::group_by(!!x, !!facet) %>%
    dplyr::summarize(
      group_sum = sum(!!y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      group_sum_label = scales::comma(
        round(group_sum, decimal)
      ) # Format with commas and rounding
    )

  # Define palette colors based on reversed levels
  palette_colors <- rev(
    RColorBrewer::brewer.pal(
      n = length(unique(data_processed[[rlang::as_label(fill)]])),
      palette_name
    )
  )

  # Generate the ggplot object
  p <- ggplot2::ggplot(data_processed,
                       ggplot2::aes(x = !!x, y = !!y, fill = !!fill)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(
      aes(label = label_text),
      position = position_stack(vjust = 0.5),
      size = 3, color = "black"
    ) +
    # Add sum value labels at the end of each bar
    geom_text(
      data = data_sum,
      aes(x = !!x, y = group_sum, label = group_sum_label),
      inherit.aes = FALSE, # Avoid inheriting aes from data_processed
      size = 3.5,
      color = "black",
      vjust = -0.5 # Position the labels above the bars
    ) +
    facet_grid(as.formula(paste("~", rlang::as_label(facet)))) +
    scale_fill_manual(values = palette_colors) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_y_continuous(labels = scales::comma) + # Apply comma formatting to y-axis
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom"
    ) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = NULL,
      y = NULL,
      fill = NULL
    )

  return(p)
}
