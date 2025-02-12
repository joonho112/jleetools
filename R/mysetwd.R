#' Set Working Directory and Return Paths
#'
#' The `mysetwd` function simplifies the process of setting a working directory
#' and returning derived paths such as `work_dir` and `data_dir` in the global
#' environment.
#'
#' @param project_name A character string specifying the name of the project directory (e.g., "MyProject").
#' @param source_dir A character string specifying the base directory containing the project (e.g., "Documents").
#'                   Defaults to `"Documents"`.
#' @param data_dir A character string specifying the name of the data subdirectory (e.g., "datasets").
#'                 Defaults to `"datasets"`.
#'
#' @return Assigns `work_dir` and `data_dir` variables to the global environment:
#' \itemize{
#'   \item `work_dir`: The main project directory path.
#'   \item `data_dir`: The data subdirectory path inside the project directory.
#' }
#' Additionally, prints messages indicating the paths for verification.
#'
#' @examples
#' \dontrun{
#' mysetwd(
#'   project_name = "Early-Childhood-Education-Policy",
#'   source_dir = "Documents",
#'   data_dir = "01_datasets"
#' )
#' }
#'
#' @export
mysetwd <- function(project_name, source_dir = "Documents", data_dir = "datasets") {
  # Construct the work_dir path
  work_dir <- file.path(path.expand("~"), source_dir, project_name)

  # Construct the data_dir path inside the work_dir
  data_dir <- file.path(work_dir, data_dir)

  # Assign the directories to the global environment
  assign("work_dir", work_dir, envir = .GlobalEnv)
  assign("data_dir", data_dir, envir = .GlobalEnv)

  # Return a message
  message("Working directory set: ", work_dir)
  message("Data directory set: ", data_dir)
}
