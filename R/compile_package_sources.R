#' Compile Package Sources into a Single Markdown File
#'
#' This function reads and concatenates an R package's `README.md`, all `.R` scripts in the `R/` folder,
#' all `.Rd` files in the `man/` folder, and all recognized source files in `src/` (e.g., `.cpp`, `.h`, `.win`).
#' The result is a single `.md` file containing all these concatenated texts.
#'
#' @param package_path A character string indicating the path to the local R package directory.
#' @param output_file A character string for the name (and optional path) of the output `.md` file.
#'
#' @return The function writes a single `.md` file (specified by \code{output_file}) containing
#'         the concatenated contents of the README, R scripts, Rd files, and C/C++ source files.
#'
#' @examples
#' \dontrun{
#' # Usage example:
#' compile_package_sources(
#'   package_path = "path/to/your/R-package",
#'   output_file  = "combined_package_sources.md"
#' )
#' }
#'
#' @export
compile_package_sources <- function(package_path,
                                    output_file = "combined_package_sources.md") {
  # package_path: directory path of the R package
  # output_file: name of the resulting .md file (can include path)

  # 1) Read README.md
  readme_path <- file.path(package_path, "README.md")
  readme_text <- ""
  if (file.exists(readme_path)) {
    cat(sprintf("Reading README from: %s\n", readme_path))
    readme_lines <- readLines(readme_path, warn = FALSE, encoding = "UTF-8")
    readme_text <- paste(readme_lines, collapse = "\n")
  } else {
    cat("No README.md found.\n")
  }

  # 2) Read all .R scripts from the R/ folder
  r_folder_path <- file.path(package_path, "R")
  r_scripts <- character(0)
  if (dir.exists(r_folder_path)) {
    # List files with the .R extension
    r_files <- list.files(
      r_folder_path,
      pattern = "\\.R$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    # Read each .R file and append to the text
    for (rf in r_files) {
      cat(sprintf("Reading R script: %s\n", rf))
      r_lines <- readLines(rf, warn = FALSE, encoding = "UTF-8")
      # Optional header to mark each file
      file_header <- paste0("\n\n## File: ", basename(rf), "\n\n")
      r_scripts <- c(r_scripts, file_header, r_lines)
    }
  } else {
    cat("No R folder found.\n")
  }
  r_scripts_text <- paste(r_scripts, collapse = "\n")

  # 3) Read all .Rd files from the man/ folder
  man_folder_path <- file.path(package_path, "man")
  rd_scripts <- character(0)
  if (dir.exists(man_folder_path)) {
    # List files with the .Rd extension
    rd_files <- list.files(
      man_folder_path,
      pattern = "\\.Rd$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    for (rdf in rd_files) {
      cat(sprintf("Reading Rd file: %s\n", rdf))
      rd_lines <- readLines(rdf, warn = FALSE, encoding = "UTF-8")
      file_header <- paste0("\n\n## Rd File: ", basename(rdf), "\n\n")
      rd_scripts <- c(rd_scripts, file_header, rd_lines)
    }
  } else {
    cat("No man folder found.\n")
  }
  rd_scripts_text <- paste(rd_scripts, collapse = "\n")

  # 4) Read all recognized source files from src/ folder
  #    We look for .cpp, .h, .win, or similar. Adjust pattern as needed.
  src_folder_path <- file.path(package_path, "src")
  src_scripts <- character(0)
  if (dir.exists(src_folder_path)) {
    # Match .cpp, .h, .win (case-insensitive). Add more extensions if needed.
    src_files <- list.files(
      src_folder_path,
      pattern = "\\.(cpp|h|win)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    for (sf in src_files) {
      cat(sprintf("Reading source file: %s\n", sf))
      s_lines <- readLines(sf, warn = FALSE, encoding = "UTF-8")
      # Optional header to mark each file
      file_header <- paste0("\n\n## Source File: ", basename(sf), "\n\n")
      src_scripts <- c(src_scripts, file_header, s_lines)
    }
  } else {
    cat("No src folder found.\n")
  }
  src_scripts_text <- paste(src_scripts, collapse = "\n")

  # 5) Combine everything in a single text
  #    README -> R scripts -> Rd files -> src files
  final_text <- paste(
    "# Combined Package Sources\n\n",
    "## 1) README.md\n\n",
    readme_text,
    "\n\n---\n",
    "## 2) R Scripts\n\n",
    r_scripts_text,
    "\n\n---\n",
    "## 3) Rd Files\n\n",
    rd_scripts_text,
    "\n\n---\n",
    "## 4) C/C++ Source Files\n\n",
    src_scripts_text,
    "\n",
    sep = ""
  )

  # 6) Write the result to output_file
  cat(sprintf("Writing all content to: %s\n", output_file))
  writeLines(final_text, con = output_file, useBytes = TRUE)
}
