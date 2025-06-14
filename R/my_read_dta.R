#' Read a Stata *.dta* file and **safely** convert labelled vectors to factors
#'
#' A robust wrapper around [haven::read_dta()] that
#' 1. *Optionally* converts **numeric `labelled`** vectors to `factor`s, **and**
#' 2. Normalises the factor levels so hidden non-breaking spaces (NBSP `U+00A0`,
#' narrow NBSP `U+202F`) and other Unicode “look-alike” characters no longer
#' break downstream comparisons.
#'
#' @param file_path `character(1)`. Path to the Stata *.dta* file.
#' @param label_to_factor `logical(1)`, default `TRUE`.
#'   * **`TRUE`** – convert numeric `labelled` vectors to `factor`s using their
#'   value labels.
#'   * **`FALSE`** – keep them as numeric; label attributes are preserved.
#'
#' @return A data frame/tibble identical to the output of
#'   `haven::read_dta()`, with optional factor conversion **and** Unicode /
#'   whitespace normalisation applied to *all* factor levels.
#'
#' @details
#' * Conversion uses [haven::as_factor()] with `levels = "labels"`.
#' * Unicode clean-up steps (performed on every factor column):
#'   1. **NFKC normalisation** – collapses visually identical glyphs
#'      (e.g. “≥” variants) to a single code-point.
#'   2. **NBSP → regular space** replacement.
#'   3. **`trimws()`** to remove leading/trailing white-space, including NBSPs.
#' * These operations rely on **stringi**, which is imported automatically.
#'
#' @seealso [haven::read_dta()], [haven::as_factor()],
#'   [labelled::is.labelled()], [stringi::stri_trans_general()]
#'
#' @examples
#' \dontrun{
#' # Default – convert labels and clean up factor levels
#' df <- my_read_dta("survey_data.dta")
#'
#' # Keep numeric codes; still cleans NBSP/Unicode in existing factors
#' df_raw <- my_read_dta("survey_data.dta", label_to_factor = FALSE)
#' }
#'
#' @export
#' @importFrom haven read_dta as_factor
#' @importFrom labelled is.labelled
#' @importFrom dplyr mutate across where
#' @importFrom stringi stri_trans_general stri_replace_all_fixed stri_trim_both
my_read_dta <- function(file_path, label_to_factor = TRUE) {
  df <- haven::read_dta(file_path)

  # ── 1. labelled → factor (if requested) -----------------------------------
  if (label_to_factor) {
    df <- dplyr::mutate(
      df,
      dplyr::across(
        dplyr::where(~ labelled::is.labelled(.x) && is.numeric(.x)),
        ~ haven::as_factor(.x, levels = "labels")
      )
    )
  }

  # ── 2. Unicode / whitespace normalisation for *all* factor columns ---------
  df <- dplyr::mutate(
    df,
    dplyr::across(
      dplyr::where(is.factor),
      ~ {
        lvl <- levels(.x)

        lvl_clean <- stringi::stri_trim_both(                              # trim
          stringi::stri_replace_all_fixed(                                 # NBSP→space
            stringi::stri_trans_general(lvl, "NFKC")                       # normalise
            , "\u00A0", " ")
        )

        # apply only if something changed
        if (!identical(lvl, lvl_clean)) levels(.x) <- lvl_clean
        .x
      }
    )
  )

  df
}
