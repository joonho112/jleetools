% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svygtsum_crosstab.R
\name{svygtsum_crosstab}
\alias{svygtsum_crosstab}
\title{Cross-tabulate a Categorical Variable (Unweighted vs Survey-Weighted)}
\usage{
svygtsum_crosstab(
  design,
  by_var,
  cat_var,
  cat_label = NULL,
  caption = NULL,
  p_digits = 2,
  spanner_unwt = "Unweighted Sample",
  spanner_wt = "Survey Weight-Adjusted",
  missing = "no"
)
}
\arguments{
\item{design}{A \code{survey.design} or \code{svyrep.design} object.}

\item{by_var}{Grouping variable to form columns (quoted or unquoted).}

\item{cat_var}{Categorical variable to tabulate (quoted or unquoted).}

\item{cat_label}{Optional character string to use as the display label for
\code{cat_var}.  Defaults to the variable name.}

\item{caption}{Optional table caption (Markdown is supported).}

\item{p_digits}{Integer; number of decimal places for \emph{p-values} (default
\code{2}).}

\item{spanner_unwt}{Title to place above the unweighted columns.  Default
\code{"Unweighted Sample"}.}

\item{spanner_wt}{Title to place above the weighted columns.  Default
\code{"Survey Weight-Adjusted"}.}

\item{missing}{Passed through to \strong{gtsummary}’s \verb{missing =} argument
(\code{"no"}, \code{"ifany"}, or \code{"always"}).  Defaults to \code{"no"}.}
}
\value{
A \code{gtsummary::tbl_merge} object that can be printed as-is or rendered
with \strong{gt}/\strong{flextable} methods for reporting.
}
\description{
Builds two \strong{gtsummary} tables from the same
\code{\link[survey:svydesign]{survey.design}} / \code{\link[survey:svrepdesign]{svyrep.design}}
object—one using raw counts (“unweighted”) and one accounting for survey
weights—then merges them side-by-side with column spanners.
This makes it easy to see at a glance how the weighting scheme affects the
distribution of a categorical variable across groups.
}
\details{
All rows with missing values in either \code{by_var} or \code{cat_var} are dropped
\emph{after} converting the design to a \strong{srvyr} object, so the survey design
settings (strata, PSU, replicate weights, etc.) are preserved.
}
\examples{
\dontrun{
library(survey)
library(srvyr)

# toy example ------------------------------------------------------------
set.seed(1)
df <- data.frame(
  w   = runif(100, 0.5, 2),
  id  = 1:100,
  sex = sample(c("Male", "Female"), 100, TRUE),
  race = sample(c("White", "Black", "Other"), 100, TRUE)
)
des <- svydesign(ids = ~id, weights = ~w, data = df)

svygtsum_crosstab(
  design  = des,
  by_var  = sex,
  cat_var = race,
  cat_label = "Race / ethnicity",
  caption   = "Table 1. Race by sex — unweighted vs weighted"
)
}

}
