#' Create a Treemap with Aggregated Subgroup Proportions
#'
#' This function generates a treemap visualization based on the input data. The treemap
#' groups data by a primary category (`type1`) and displays subgroups (`type2`) with sizes
#' proportional to their values (`value`). The primary group's proportions are labeled
#' as percentages.
#'
#' @param data A data frame containing the data for the treemap.
#' @param type1 The primary grouping variable (unquoted).
#' @param type2 The secondary grouping variable (unquoted).
#' @param value The numeric variable determining the area size (unquoted).
#' @param type2_label A character string specifying the legend title for `type2`. Defaults to "Category".
#'
#' @return A ggplot object representing the treemap.
#'
#' @details This function uses the {ggplot2} and {treemapify} packages to generate
#' treemaps. Primary group percentages are calculated and displayed within their
#' respective tiles.
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(treemapify)
#' library(see)
#'
#' # Example data
# df <- data.frame(
#   type1 = c("A", "A", "B", "B"),
#   type2 = c("A1", "A2", "B1", "B2"),
#   value = c(30, 70, 50, 50)
# )
#'
#' # Generate the treemap
#' plot_treemap(df, type1, type2, value, type2_label = "Subcategories")
#' }
#'
#' @importFrom dplyr group_by summarize ungroup mutate full_join
#' @importFrom ggplot2 ggplot aes labs theme scale_fill_manual
#' @importFrom treemapify geom_treemap geom_treemap_subgroup_border geom_treemap_subgroup_text
#' @importFrom rlang enquo quo_name
#' @importFrom see scale_fill_okabeito
#' @export
plot_treemap <- function(data, type1, type2, value, type2_label = "Category") {
  # Ensure column names are symbols
  type1 <- enquo(type1)
  type2 <- enquo(type2)
  value <- enquo(value)

  # Aggregate data for type1 proportions
  df_aggr <- data %>%
    group_by(!!type1) %>%
    summarize(
      sum_value = sum(!!value, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      aggr_sum = sum(sum_value),
      percent = (sum_value / aggr_sum) * 100,
      pct_string = paste0(format(round(percent, 1), nsmall = 1), "%"),
      type1_pct = paste(as.character(!!type1), pct_string, sep = "\n")
    )

  # Merge aggregated data back to the original data
  df_plot <- full_join(
    data, df_aggr,
    by = setNames("type1", quo_name(type1))
  )

  # Create treemap
  p <- df_plot %>%
    ggplot(
      aes(
        area = !!value,
        subgroup = type1_pct,
        fill = !!type2,
        label = !!type2
      )
    ) +
    # Draw type_2 borders and fill colors
    treemapify::geom_treemap() +
    scale_fill_okabeito() +

    # Draw type_1 borders
    treemapify::geom_treemap_subgroup_border(
      layout = "squarified",
      color = "black"
    ) +

    # Print type_1 text
    treemapify::geom_treemap_subgroup_text(
      place = "centre",
      reflow = TRUE,
      colour = "white",
      min.size = 0
    ) +
    theme(legend.position = "bottom") +
    labs(
      fill = type2_label
    )

  return(p)
}
