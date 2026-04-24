#' Convert Labelled Data Frame to Tibble
#'
#' Converts a \code{labelled} data frame (typically imported via
#' \code{haven}) to a clean \code{tibble} following tidyverse conventions.
#' Labelled vectors are converted to factors, character columns are
#' whitespace-trimmed, \code{POSIXt} columns are converted to \code{Date},
#' and all Haven-specific metadata (empty strings, formats, variable labels,
#' column widths) are stripped.
#'
#' @param labelled_data A \code{data.frame} or \code{tibble} with
#'   \code{labelled} columns, as typically produced by \code{haven::read_sav()}
#'   or similar.
#' @param check_varlabels An optional named character vector where names are
#'   column names and values are the expected variable labels. If provided,
#'   the function checks that each specified column carries exactly the
#'   expected label and throws an informative error if any mismatch is found.
#'   If \code{NULL} (the default), no label checking is performed.
#'
#' @return A \code{tibble} derived from \code{labelled_data} with the
#'   following transformations applied:
#'   \itemize{
#'     \item \code{labelled} columns converted to factors via
#'       \code{labelled::to_factor()}.
#'     \item Character columns trimmed and squished via
#'       \code{stringr::str_squish()}.
#'     \item \code{POSIXt} columns converted to \code{Date} via
#'       \code{lubridate::as_date()}.
#'     \item Empty strings, Haven formats, variable labels, and column
#'       widths removed.
#'   }
#'
#' @details
#' The function temporarily widens the warning message length and console
#' width options to ensure that long variable label mismatch errors are
#' printed in full rather than truncated. These options are restored on exit
#' via \code{on.exit()}.
#'
#' @examples
#' \dontrun{
#' raw <- haven::read_sav("survey.sav")
#'
#' # Without label checking
#' clean <- labelled_2_tibble(raw)
#'
#' # With label checking
#' expected_labels <- c(q1 = "Satisfaction score", q2 = "Trust score")
#' clean <- labelled_2_tibble(raw, check_varlabels = expected_labels)
#' }
#'
#' @importFrom dplyr mutate across filter left_join join_by pull
#' @importFrom labelled is.labelled to_factor get_variable_labels
#' @importFrom haven zap_empty zap_formats zap_label zap_widths
#' @importFrom stringr str_squish
#' @importFrom lubridate as_date
#' @importFrom tibble enframe
#'
#' @export
labelled_2_tibble <- function(labelled_data, check_varlabels = NULL) {

   # Input check --------------------------------------------------------------
   if (!is.data.frame(labelled_data)) {
      stop("`labelled_data` must be a data.frame or tibble.")
      }
   if (!is.null(check_varlabels) && !is.character(check_varlabels)) {
      stop("`check_varlabels` must be a named character vector or NULL.")
      }
   if (!is.null(check_varlabels) && is.null(names(check_varlabels))) {
      stop("`check_varlabels` must be a named character vector.")
      }

   # Temporarily widen output options for readable error messages -------------
   old <- options(warning.length = 8170, width = 10000)
   on.exit(options(old), add = TRUE)

   # Variable label check -----------------------------------------------------
   if (!is.null(check_varlabels)) {

      # Check that all names in check_varlabels exist in labelled_data
      missing_vars <- setdiff(names(check_varlabels), names(labelled_data))
      if (length(missing_vars) > 0)
         warning("Variables in `check_varlabels` not found in `labelled_data`: ",
                 paste(missing_vars, collapse = ", "))

      tmp1 <- check_varlabels |>
         tibble::enframe(value = "required_value")

      tmp2 <- labelled::get_variable_labels(labelled_data) |>
         tibble::enframe(value = "value") |>
         tidyr::unnest(value) |>
         dplyr::left_join(tmp1, dplyr::join_by(name)) |>
         dplyr::filter(!is.na(required_value)) |>       # only check requested vars
         dplyr::filter(value != required_value)         # keep mismatches only

      if (nrow(tmp2) > 0) {
         errors <- tmp2 |>
            dplyr::mutate(
               error = sprintf('\n  %s: found "%s", expected "%s"',
                               name, value, required_value)
            ) |>
            dplyr::pull(error)
         stop("Wrong variable labels for the following variables:", errors)
      }
   }

   # Convert labelled data to tibble ------------------------------------------
   tibbled_data <- labelled_data |>
      dplyr::mutate(
         dplyr::across(where(labelled::is.labelled), labelled::to_character),
         dplyr::across(where(is.character),          stringr::str_squish),
         dplyr::across(where(is.factor),\(x) forcats::fct_relabel(x, stringr::str_squish)),
         dplyr::across(where(lubridate::is.POSIXt),  lubridate::as_date)
         ) |>
      haven::zap_formats() |>
      haven::zap_label()   |>
      haven::zap_widths()

   return(tibbled_data)
}