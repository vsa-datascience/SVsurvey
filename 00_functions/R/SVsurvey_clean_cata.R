#' Clean Check-All-That-Apply Question (Without "None" Option)
#'
#' Internal helper that recodes a set of binary (\code{"yes"}/\code{NA})
#' check-all-that-apply variables and their associated don't-know variable,
#' for questions that do not have a "none of the above" option.
#'
#' @param .data A \code{data.frame} or \code{tibble}.
#' @param .var A character vector of column names representing the individual
#'   answer options (expected values: \code{"yes"} or \code{NA}).
#' @param .dk A single character string naming the don't-know column.
#'
#' @return The input \code{.data} with recoded columns. For each variable in
#'   \code{.var}:
#'   \itemize{
#'     \item \code{"yes"} if the respondent checked this option.
#'     \item \code{"dkn"} if the don't-know option was selected.
#'     \item \code{"no"} if at least one other option was checked.
#'     \item \code{NA} otherwise.
#'   }
#'
#' @keywords internal
SVsurvey_clean_cata_without_none <- function(.data, .var, .dk) {

   # Input checks -------------------------------------------------------------
   if (!is.data.frame(.data))
      stop("`.data` must be a data.frame or tibble.")
   if (!is.character(.var) || length(.var) == 0)
      stop("`.var` must be a non-empty character vector.")
   if (!is.character(.dk) || length(.dk) != 1)
      stop("`.dk` must be a single character string.")
   if (!.dk %in% names(.data))
      stop("Don't-know column '", .dk, "' not found in `.data`.")

   dplyr::mutate(.data,
      dplyr::across(dplyr::any_of(c(.var, .dk)), ~ dplyr::na_if(.x, "no")),
      tmp_any = dplyr::if_any(dplyr::any_of(.var), ~ .x == "yes"),
      dplyr::across(dplyr::any_of(.var), ~ dplyr::case_when(
         .x == "yes"                 ~ "yes",
         !!rlang::sym(.dk) == "yes"  ~ "dkn",
         tmp_any                     ~ "no"
         )),
      {{ .dk }} := dplyr::case_when(
         !!rlang::sym(.dk) == "yes" ~ "yes",
         tmp_any                    ~ "no"
         ),
      tmp_any = NULL
      )
   }


#' Clean Check-All-That-Apply Question (With "None" Option)
#'
#' Internal helper that recodes a set of binary (\code{"yes"}/\code{NA})
#' check-all-that-apply variables together with their associated "none of the
#' above" and don't-know variables.
#'
#' @param .data A \code{data.frame} or \code{tibble}.
#' @param .var A character vector of column names representing the individual
#'   answer options (expected values: \code{"yes"} or \code{NA}).
#' @param .dk A single character string naming the don't-know column.
#' @param .none A single character string naming the "none of the above" column.
#'
#' @return The input \code{.data} with recoded columns. For each variable in
#'   \code{.var}:
#'   \itemize{
#'     \item \code{"yes"} if the respondent checked this option.
#'     \item \code{"dkn"} if the don't-know option was selected.
#'     \item \code{"no"} if the "none" option was checked or any other option
#'       was checked.
#'     \item \code{NA} otherwise.
#'   }
#'   The \code{.none} column is recoded to \code{"yes"} if originally checked,
#'   \code{"no"} if any option was checked, \code{"dkn"} if don't-know was
#'   checked, and \code{NA} otherwise.
#'
#' @keywords internal
SVsurvey_clean_cata_with_none <- function(.data, .var, .dk, .none) {

   # Input checks -------------------------------------------------------------
   if (!is.data.frame(.data))
      stop("`.data` must be a data.frame or tibble.")
   if (!is.character(.var) || length(.var) == 0)
      stop("`.var` must be a non-empty character vector.")
   if (!is.character(.dk) || length(.dk) != 1)
      stop("`.dk` must be a single character string.")
   if (!is.character(.none) || length(.none) != 1)
      stop("`.none` must be a single character string.")
   if (!.dk %in% names(.data))
      stop("Don't-know column '", .dk, "' not found in `.data`.")
   if (!.none %in% names(.data))
      stop("None column '", .none, "' not found in `.data`.")

   dplyr::mutate(.data,
      dplyr::across(dplyr::any_of(c(.var, .dk, .none)), ~ dplyr::na_if(.x, "no")),
      tmp_any = dplyr::if_any(dplyr::any_of(.var), ~ .x == "yes"),
      dplyr::across(dplyr::any_of(.var), ~ dplyr::case_when(
         .x == "yes"                  ~ "yes",
         !!rlang::sym(.dk) == "yes"   ~ "dkn",
         !!rlang::sym(.none) == "yes" ~ "no",
         tmp_any                      ~ "no"
         )),
      {{ .none }} := dplyr::case_when(
         tmp_any                      ~ "no",
         !!rlang::sym(.dk) == "yes"   ~ "dkn",
         !!rlang::sym(.none) == "yes" ~ "yes"
         ),
      {{ .dk }} := dplyr::case_when(
         !!rlang::sym(.dk) == "yes"   ~ "yes",
         tmp_any                      ~ "no",
         !!rlang::sym(.none) == "yes" ~ "no"
         ),
      tmp_any = NULL
      )
   }


#' Clean Check-All-That-Apply Questions
#'
#' Generic wrapper that cleans check-all-that-apply (CATA) survey questions
#' by recoding binary (\code{"yes"}/\code{NA}) response columns into consistent
#' \code{"yes"}/\code{"no"}/\code{"dkn"}/\code{NA} values. Dispatches to
#' either \code{\link{SVsurvey_clean_cata_without_none}} or
#' \code{\link{SVsurvey_clean_cata_with_none}} depending on whether a
#' "none of the above" column is present.
#'
#' @param .data A \code{data.frame} or \code{tibble}.
#' @param .var A character vector of column names representing the individual
#'   answer options.
#' @param .dk A single character string naming the don't-know column.
#' @param .none A single character string naming the "none of the above"
#'   column, or \code{NULL} (the default) if no such column exists.
#'
#' @return The input \code{.data} with recoded CATA columns. Returns
#'   \code{.data} unchanged with a warning if none of \code{.var} or
#'   \code{.dk} are found in the data.
#'
#' @details
#' Cleaning is only applied when at least one variable from \code{.var} and
#' the \code{.dk} column are present in \code{.data}. If \code{.none} is
#' provided and exists in \code{.data}, the "with none" variant is used;
#' otherwise the "without none" variant is used.
#'
#' @examples
#' \dontrun{
#' # Without "none" option
#' tidyr::crossing(v1 = c("yes", NA), v2 = c("yes", NA), dk = c("yes", NA)) |>
#'    SVsurvey_clean_cata(.var = c("v1", "v2"), .dk = "dk")
#'
#' # With "none" option
#' tidyr::crossing(v1 = c("yes", NA), v2 = c("yes", NA),
#'                 none = c("yes", NA), dk = c("yes", NA)) |>
#'    SVsurvey_clean_cata(.var = c("v1", "v2"), .dk = "dk", .none = "none")
#' }
#'
#' @seealso \code{\link{SVsurvey_clean_cata_without_none}},
#'   \code{\link{SVsurvey_clean_cata_with_none}}
#'
#' @keywords internal
SVsurvey_clean_cata <- function(.data, .var, .dk, .none = NULL) {

   # Input checks -------------------------------------------------------------
   if (!is.data.frame(.data))
      stop("`.data` must be a data.frame or tibble.")

   var_present  <- any(.var %in% names(.data))
   dk_present   <- .dk %in% names(.data)
   none_present <- !is.null(.none) && .none %in% names(.data)

   if (!var_present || !dk_present) {
      return(.data)
      }

   if (none_present) {
      .data <- SVsurvey_clean_cata_with_none(.data, .var, .dk, .none)
   } else {
      .data <- SVsurvey_clean_cata_without_none(.data, .var, .dk)
      }

   return(.data)

   }
