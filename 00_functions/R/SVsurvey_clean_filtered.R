#' Clean Filtered Survey Questions
#'
#' Sets the values of filtered (conditionally asked) variables to a
#' not-applicable value for respondents who did not meet the filter condition.
#'
#' @param .data A \code{data.frame} or \code{tibble}.
#' @param .filtered A character vector of column names representing the
#'   variables that were only asked to a filtered subset of respondents.
#' @param .filter A logical expression (unquoted) that evaluates to \code{TRUE}
#'   for respondents who were in scope for the filtered questions and
#'   \code{FALSE} or \code{NA} for those who were not. Passed via
#'   \code{\link[rlang]{embrace-operator}} (\code{{ }}).
#' @param .nvt A single character string used as the not-applicable value
#'   assigned to out-of-scope respondents. Defaults to \code{"na"}.
#'
#' @return The input \code{.data} with \code{.filtered} columns set to
#'   \code{.nvt} for all rows where \code{.filter} evaluates to \code{FALSE}
#'   or \code{NA}. Returns \code{.data} unchanged with a warning if none of
#'   \code{.filtered} are present in the data.
#'
#' @details
#' Only columns from \code{.filtered} that actually exist in \code{.data}
#' are processed (via \code{dplyr::any_of()}), so it is safe to pass a
#' superset of variable names.
#'
#' \code{.filter} can be any expression that resolves to a logical vector
#' within the context of \code{.data}, such as a single column name
#' (\code{asked_q5}), a comparison (\code{age >= 18}), or a compound
#' condition (\code{age >= 18 & country == "BE"}).
#'
#' @examples
#' \dontrun{
#' # Single column as filter
#' data |>
#'   SVsurvey_clean_filtered(
#'     .filtered = c("q5a", "q5b"),
#'     .filter   = asked_q5
#'   )
#'
#' # Expression as filter
#' data |>
#'   SVsurvey_clean_filtered(
#'     .filtered = c("q5a", "q5b"),
#'     .filter   = age >= 18 & country == "BE"
#'   )
#' }
#'
#' @importFrom dplyr mutate across any_of if_else
#'
#' @export
SVsurvey_clean_filtered <- function(.data, .filtered, .filter, .nvt = "na") {

   # Input checks -------------------------------------------------------------
   if (!is.data.frame(.data))
      stop("`.data` must be a data.frame or tibble.")
   if (!is.character(.filtered) || length(.filtered) == 0)
      stop("`.filtered` must be a non-empty character vector.")

   if (!any(.filtered %in% names(.data))) {
      return(.data)
      }

   .data <- .data |>
      dplyr::mutate(
         dplyr::across(
            dplyr::any_of(.filtered),
            ~ dplyr::if_else({{ .filter }}, .x, .nvt)
            )
         )

   return(.data)
   }