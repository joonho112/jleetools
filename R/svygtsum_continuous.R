#' Summarise a Continuous Variable (Unweighted vs Survey-Weighted)
#'
#' Builds two **gtsummary** tables for a *continuous* variable—one using raw
#' (unweighted) statistics and one accounting for survey weights—then merges
#' them side-by-side with column spanners so differences are immediately
#' visible.  The interface and styling mirror
#' [`svygtsum_crosstab()`][jleetools::svygtsum_crosstab] for categorical data.
#'
#' @inheritParams svygtsum_crosstab
#' @param cont_var   Continuous variable to summarise (quoted or unquoted).
#' @param cont_label Optional display label for `cont_var`. Defaults to the
#'   variable name.
#' @param statistic_fmt Character vector defining one or more summary lines,
#'   in **gtsummary** glue syntax.  Default shows mean (±sd), median (p25, p75),
#'   and min, max:
#'   ```
#'   c("{mean} ({sd})",
#'     "{median} ({p25}, {p75})",
#'     "{min}, {max}")
#'   ```
#'
#' @return A `gtsummary::tbl_merge` object.
#'
#' @section Notes:
#' * Rows with `NA` in either `by_var` or `cont_var` are dropped **after**
#'   conversion to `srvyr::survey_tbl`, so survey design information is
#'   preserved.
#' * P-values are formatted with [gtsummary::style_pvalue()] to `p_digits`
#'   decimals.
#'
#' @examples
#' \dontrun{
#' tbl_temp <- svygtsum_continuous(
#'   design     = df_svy,
#'   by_var     = program_type,
#'   cont_var   = HB9_CESD7_TOT,
#'   cont_label = "CES-D-7 Total Score (0–21)",
#'   caption    = "CES-D-7 score by program type — unweighted vs weighted"
#' )
#' }
#'
#' @importFrom dplyr filter all_of
#' @importFrom rlang ensym as_string .data
#' @importFrom gtsummary tbl_summary tbl_svysummary tbl_merge add_p
#'   all_continuous modify_header modify_caption style_pvalue
#' @importFrom srvyr as_survey
#' @export
svygtsum_continuous <- function(
    design,
    by_var,
    cont_var,
    cont_label      = NULL,
    caption         = NULL,
    p_digits        = 2,
    spanner_unwt    = "Unweighted Sample",
    spanner_wt      = "Survey Weight-Adjusted",
    statistic_fmt   = c(
      "{mean} ({sd})",
      "{median} ({p25}, {p75})",
      "{min}, {max}"
    ),
    missing         = "no"
) {
  # ── 0. Dependency & argument checks ────────────────────────────────────────
  need_pkgs <- c("survey", "srvyr", "dplyr", "gtsummary")
  for (p in need_pkgs)
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("Package \"%s\" must be installed.", p), call. = FALSE)

  if (!inherits(design, c("survey.design", "svyrep.design")))
    stop("`design` must be a survey.design or svyrep.design object.", call. = FALSE)

  # ── 1. Tidy variable capture (quoted or bare) ─────────────────────────────
  by_sym   <- rlang::ensym(by_var)
  cont_sym <- rlang::ensym(cont_var)
  by_chr   <- rlang::as_string(by_sym)
  cont_chr <- rlang::as_string(cont_sym)
  cont_label <- cont_label %||% cont_chr

  # ── 2. Drop NAs but keep design integrity ────────────────────────────────
  des_nomiss <- srvyr::as_survey(design) |>
    dplyr::filter(!is.na(.data[[by_chr]]), !is.na(.data[[cont_chr]]))

  df_unwt <- des_nomiss$variables

  # p-value formatter
  fmt_p <- function(x) gtsummary::style_pvalue(x, digits = p_digits)

  # ── 3. Unweighted summary ────────────────────────────────────────────────
  tbl_unwt <- gtsummary::tbl_summary(
    data      = df_unwt,
    by        = dplyr::all_of(by_chr),
    include   = dplyr::all_of(cont_chr),
    label     = setNames(list(cont_label), cont_chr),
    type      = gtsummary::all_continuous() ~ "continuous2",
    statistic = gtsummary::all_continuous() ~ statistic_fmt,
    missing   = missing
  ) |>
    gtsummary::add_p(pvalue_fun = fmt_p)

  # ── 4. Weighted summary ─────────────────────────────────────────────────
  tbl_wt <- gtsummary::tbl_svysummary(
    data      = des_nomiss,
    by        = dplyr::all_of(by_chr),
    include   = dplyr::all_of(cont_chr),
    label     = setNames(list(cont_label), cont_chr),
    type      = gtsummary::all_continuous() ~ "continuous2",
    statistic = gtsummary::all_continuous() ~ statistic_fmt,
    missing   = missing
  ) |>
    gtsummary::add_p(pvalue_fun = fmt_p)

  # ── 5. Merge & final formatting ─────────────────────────────────────────
  gtsummary::tbl_merge(
    tbls        = list(tbl_unwt, tbl_wt),
    tab_spanner = paste0("**", c(spanner_unwt, spanner_wt), "**")
  ) |>
    gtsummary::modify_header(label ~ "**Variable**") |>
    gtsummary::modify_caption(caption)
}

