#' Save a ggplot Object in Multiple Formats
#'
#' The `myggsave` function saves a given ggplot object in multiple file formats (PDF, PNG, and JPEG).
#' It provides a convenient way to export high-quality visualizations for various purposes.
#'
#' @param p A ggplot object to save.
#' @param filename A character string specifying the base filename (without extension) for the saved files.
#' @param w Width of the output plots in inches. Defaults to 10.
#' @param h Height of the output plots in inches. Defaults to 6.
#'
#' @details
#' This function saves the plot in three formats: PDF, PNG, and JPEG.
#' - PNG and JPEG are saved at a resolution of 300 DPI.
#' - PDF maintains vector graphics for scalability.
#'
#' If the specified `filename` already includes an extension, the function will overwrite it with the appropriate extensions for each format.
#'
#' @return Invisible `NULL`. The function is used for its side effects of saving files.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' myggsave(p, "my_plot")
#' }
#'
#' @importFrom grDevices pdf png jpeg dev.off
#' @export
myggsave <- function(p, filename, w = 10, h = 6) {
  # Validate inputs
  if (!inherits(p, "ggplot")) {
    stop("The input `p` must be a ggplot object.")
  }
  if (!is.character(filename) || length(filename) != 1) {
    stop("The `filename` must be a single character string.")
  }

  # Define the formats and devices
  formats <- c("pdf", "png", "jpeg")
  devices <- list(
    pdf = function(file, w, h) { grDevices::pdf(file, width = w, height = h) },
    png = function(file, w, h) { grDevices::png(file, width = w, height = h, units = "in", res = 300) },
    jpeg = function(file, w, h) { grDevices::jpeg(file, width = w, height = h, units = "in", res = 300) }
  )

  # Ensure filename has no existing extensions
  base_name <- sub("\\.[a-zA-Z]+$", "", filename)

  # Save the plot in all formats
  for (format in formats) {
    file_name <- paste0(base_name, ".", format)
    devices[[format]](file_name, w, h) # Open device
    tryCatch({
      print(p) # Render the plot
    }, error = function(e) {
      warning(paste("Failed to save plot as", format, ":", e$message))
    }, finally = {
      dev.off() # Close device
    })
  }

  message("Plot saved in formats: ", paste(formats, collapse = ", "))
  invisible(NULL)
}
