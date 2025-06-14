#' Cross-tabulate a Categorical Variable (Unweighted vs Survey-Weighted)
#'
#' Builds two **gtsummary** tables from the same
#' [`survey.design`][survey::svydesign] / [`svyrep.design`][survey::svrepdesign]
#' object—one using raw counts (“unweighted”) and one accounting for survey
#' weights—then merges them side-by-side with column spanners.
#' This makes it easy to see at a glance how the weighting scheme affects the
#' distribution of a categorical variable across groups.
#'
#' All rows with missing values in either `by_var` or `cat_var` are dropped
#' *after* converting the design to a **srvyr** object, so the survey design
#' settings (strata, PSU, replicate weights, etc.) are preserved.
#'
#' @param design A `survey.design` or `svyrep.design` object.
#' @param by_var Grouping variable to form columns (quoted or unquoted).
#' @param cat_var Categorical variable to tabulate (quoted or unquoted).
#' @param cat_label Optional character string to use as the display label for
#'   `cat_var`.  Defaults to the variable name.
#' @param caption Optional table caption (Markdown is supported).
#' @param p_digits Integer; number of decimal places for *p-values* (default
#'   `2`).
#' @param spanner_unwt Title to place above the unweighted columns.  Default
#'   `"Unweighted Sample"`.
#' @param spanner_wt Title to place above the weighted columns.  Default
#'   `"Survey Weight-Adjusted"`.
#' @param missing Passed through to **gtsummary**’s `missing =` argument
#'   (`"no"`, `"ifany"`, or `"always"`).  Defaults to `"no"`.
#'
#' @return A `gtsummary::tbl_merge` object that can be printed as-is or rendered
#'   with **gt**/**flextable** methods for reporting.
#'
#' @examples
#' \dontrun{
#' library(survey)
#' library(srvyr)
#'
#' # toy example ------------------------------------------------------------
#' set.seed(1)
#' df <- data.frame(
#'   w   = runif(100, 0.5, 2),
#'   id  = 1:100,
#'   sex = sample(c("Male", "Female"), 100, TRUE),
#'   race = sample(c("White", "Black", "Other"), 100, TRUE)
#' )
#' des <- svydesign(ids = ~id, weights = ~w, data = df)
#'
#' svygtsum_crosstab(
#'   design  = des,
#'   by_var  = sex,
#'   cat_var = race,
#'   cat_label = "Race / ethnicity",
#'   caption   = "Table 1. Race by sex — unweighted vs weighted"
#' )
#' }
#'
#' @importFrom dplyr filter all_of
#' @importFrom rlang ensym as_string .data
#' @importFrom gtsummary tbl_summary tbl_svysummary tbl_merge add_p
#'   all_categorical modify_header modify_caption style_pvalue
#' @importFrom srvyr as_survey
#' @export
svygtsum_crosstab <- function(
    design,
    by_var,
    cat_var,
    cat_label      = NULL,
    caption        = NULL,
    p_digits       = 2,
    spanner_unwt   = "Unweighted Sample",
    spanner_wt     = "Survey Weight-Adjusted",
    missing        = "no"
) {
  # ── 0. Dependency & argument checks ────────────────────────────────────────
  need_pkgs <- c("survey", "srvyr", "dplyr", "gtsummary")
  for (p in need_pkgs) {
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("Package “%s” must be installed.", p), call. = FALSE)
  }
  if (!inherits(design, c("survey.design", "svyrep.design")))
    stop("`design` must be a survey.design or svyrep.design object.", call. = FALSE)

  # ── 1. Tidy variable capture (quoted or bare) ──────────────────────────────
  by_sym  <- rlang::ensym(by_var)
  cat_sym <- rlang::ensym(cat_var)
  by_chr  <- rlang::as_string(by_sym)
  cat_chr <- rlang::as_string(cat_sym)
  cat_label <- cat_label %||% cat_chr

  # ── 2. Drop NAs while retaining design information ────────────────────────
  des_nomiss <- srvyr::as_survey(design) |>
    dplyr::filter(!is.na(.data[[by_chr]]), !is.na(.data[[cat_chr]]))
  df_unwt <- des_nomiss$variables

  # Wrapper for nicely-formatted p-values
  fmt_p <- function(x) gtsummary::style_pvalue(x, digits = p_digits)

  # ── 3. Unweighted table ────────────────────────────────────────────────────
  tbl_unwt <- gtsummary::tbl_summary(
    data      = df_unwt,
    by        = dplyr::all_of(by_chr),
    include   = dplyr::all_of(cat_chr),
    label     = setNames(list(cat_label), cat_chr),
    statistic = list(gtsummary::all_categorical() ~ "{n} ({p}%)"),
    missing   = missing
  ) |>
    gtsummary::add_p(pvalue_fun = fmt_p)

  # ── 4. Weighted table ──────────────────────────────────────────────────────
  tbl_wt <- gtsummary::tbl_svysummary(
    data      = des_nomiss,
    by        = dplyr::all_of(by_chr),
    include   = dplyr::all_of(cat_chr),
    label     = setNames(list(cat_label), cat_chr),
    statistic = list(gtsummary::all_categorical() ~ "{n} ({p}%)"),
    missing   = missing
  ) |>
    gtsummary::add_p(pvalue_fun = fmt_p)

  # ── 5. Merge & final formatting ────────────────────────────────────────────
  gtsummary::tbl_merge(
    tbls        = list(tbl_unwt, tbl_wt),
    tab_spanner = paste0("**", c(spanner_unwt, spanner_wt), "**")
  ) |>
    gtsummary::modify_header(label ~ "**Variable**") |>
    gtsummary::modify_caption(caption)
}
