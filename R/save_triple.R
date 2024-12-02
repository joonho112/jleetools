#' Save Data Frame to Multiple File Formats
#'
#' `save_triple` saves a data frame to three different file formats: `.rds`, `.dta`, and `.xlsx`.
#' This function ensures that column names are sanitized for compatibility across formats
#' and handles errors gracefully for each format.
#'
#' @param df A data frame or tibble to be saved.
#' @param file_name A base file name (without extension) to save the files. If an extension is included,
#' it will be replaced with `.rds`, `.dta`, and `.xlsx`.
#'
#' @details
#' - **RDS**: Saves the data frame in R's native serialized format using `readr::write_rds()`.
#' - **DTA**: Saves the data frame in Stata's `.dta` format using `haven::write_dta()`.
#' - **XLSX**: Saves the data frame in Excel format using `writexl::write_xlsx()`.
#'
#' Column names are automatically sanitized by replacing invalid characters (non-alphanumeric or underscores)
#' with underscores (`_`) for compatibility across all file formats.
#'
#' @return Invisible `NULL`. Files are written to the working directory or specified path.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   id = 1:5,
#'   name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
#'   value = c(10.5, 20.3, 30.1, 40.2, 50.4)
#' )
#'
#' # Save to multiple formats
#' save_triple(df, "example_data")
#'}
#'
#' @importFrom readr write_rds
#' @importFrom haven write_dta
#' @importFrom writexl write_xlsx
#' @importFrom stringr str_remove str_replace_all str_ends
#' @export
save_triple <- function(df, file_name) {
  # Ensure input is a data frame
  if (!is.data.frame(df)) {
    stop("The input `df` must be a data frame or tibble.")
  }

  # Ensure file_name is a string
  if (!is.character(file_name) || length(file_name) != 1) {
    stop("The `file_name` must be a single string.")
  }

  # Construct file names with appropriate extensions
  base_name <- stringr::str_remove(file_name, "\\.[^.]+$") # Remove existing extension if any
  rds_file <- paste0(base_name, ".rds")
  dta_file <- paste0(base_name, ".dta")
  xlsx_file <- paste0(base_name, ".xlsx")

  # Replace invalid characters in column names for compatibility
  safe_names <- stringr::str_replace_all(names(df), "[^A-Za-z0-9_]", "_")
  if (any(names(df) != safe_names)) {
    warning("Column names were modified to be compatible with all formats.")
    names(df) <- safe_names
  }

  # Attempt to write files and catch potential errors
  tryCatch({
    readr::write_rds(df, rds_file)
    message("Saved RDS file: ", rds_file)
  }, error = function(e) {
    warning("Failed to save RDS file: ", e$message)
  })

  tryCatch({
    haven::write_dta(df, dta_file)
    message("Saved DTA file: ", dta_file)
  }, error = function(e) {
    warning("Failed to save DTA file: ", e$message)
  })

  tryCatch({
    writexl::write_xlsx(df, xlsx_file)
    message("Saved XLSX file: ", xlsx_file)
  }, error = function(e) {
    warning("Failed to save XLSX file: ", e$message)
  })

  invisible(NULL)
}
