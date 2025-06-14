#' Save a **gtsummary** Table as a Markdown (`.md`) File
#'
#' Converts a `gtsummary` table to Markdown via
#' [`as_kable_extra()`][gtsummary::as_kable_extra] and writes it to disk so the
#' table can be embedded in README files, Quarto/R Markdown documents, or issue
#' comments.  If the output directory does **not** yet exist it is created
#' recursively.
#'
#' @param tbl   A `gtsummary` table (e.g.\ `tbl_summary`, `tbl_regression`,
#'   `tbl_merge`, …).
#' @param file  File path for the Markdown file. Default `"table.md"`.  The
#'   function ensures the path ends with the `.md` extension.
#' @param ...   Additional arguments passed to
#'   [`gtsummary::as_kable_extra()`], such as `booktabs = TRUE`,
#'   `caption = "My table"`\ .
#'
#' @return (Invisibly) the normalized path to the written file, **invisibly**.
#'   A message noting the save location is printed for convenience.
#'
#' @section Error-handling:
#' * Checks that **gtsummary** *and* **kableExtra** are installed.
#' * Verifies `tbl` inherits from class `"gtsummary"`.
#' * Ensures the output directory exists or creates it.
#'
#' @examples
#' \dontrun{
#' library(gtsummary)
#' tbl <- tbl_summary(iris, by = Species)
#'
#' # Save with default name in working directory
#' save_gtsummary_md(tbl)
#'
#' # Save to a docs/ folder with custom kableExtra options
#' save_gtsummary_md(
#'   tbl,
#'   file = "docs/iris_table.md",
#'   booktabs = TRUE,
#'   caption  = "Table 1. Iris summary"
#' )
#' }
#'
#' @importFrom gtsummary as_kable_extra
#' @export
save_gtsummary_md <- function(tbl, file = "table.md", ...) {
  ## ── 0. Dependency checks ────────────────────────────────────────────────
  if (!requireNamespace("gtsummary", quietly = TRUE))
    stop("Package \"gtsummary\" must be installed.", call. = FALSE)
  if (!requireNamespace("kableExtra", quietly = TRUE))
    stop("Package \"kableExtra\" must be installed for Markdown export.",
         call. = FALSE)

  ## ── 1. Validate input table ─────────────────────────────────────────────
  if (!inherits(tbl, "gtsummary"))
    stop("`tbl` must be a gtsummary object.", call. = FALSE)

  ## ── 2. Normalise & prepare output path ──────────────────────────────────
  if (!grepl("\\.md$", file, ignore.case = TRUE))
    file <- paste0(file, ".md")

  out_path <- normalizePath(file, winslash = "/", mustWork = FALSE)
  out_dir  <- dirname(out_path)

  if (!dir.exists(out_dir))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  ## ── 3. Convert to Markdown ──────────────────────────────────────────────
  md_txt <- gtsummary::as_kable_extra(tbl, format = "markdown", ...) |>
    paste(collapse = "\n")

  ## ── 4. Write file ───────────────────────────────────────────────────────
  writeLines(md_txt, out_path, useBytes = TRUE)
  message("✓ Markdown saved: ", out_path)

  invisible(out_path)
}
