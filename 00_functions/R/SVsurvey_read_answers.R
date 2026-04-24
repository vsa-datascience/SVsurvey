#' Read Survey Answers Metadata
#'
#' Reads answer value labels from the shared \code{Answers.xlsx} file and
#' returns the most recent version of each answer value that applies to the
#' requested survey edition.
#'
#' @param the_surv_id A character string specifying the survey ID. Used to
#'   filter answers to those introduced at or before this edition.
#'
#' @return A \code{tibble} with one row per answer value, containing the most
#'   recent label applicable to \code{the_surv_id}. Columns include at
#'   minimum:
#'   \describe{
#'     \item{value}{Numeric or character answer code}
#'     \item{vallabel}{Human-readable label for the answer value}
#'   }
#'   Note: \code{from_surv_id} is dropped from the returned tibble.
#'
#' @details
#' The function reads from the fixed path
#' \code{03_process_editions/Metadata/Answers.xlsx}, which is a longitudinal
#' registry of answer value labels. Each row represents a version of a label
#' for a given answer value, introduced from a specific survey edition onwards
#' (\code{from_surv_id}).
#'
#' The filtering logic applies two steps:
#' \enumerate{
#'   \item Retains only rows where \code{from_surv_id <= the_surv_id},
#'     i.e. label versions that were already in use by this edition.
#'   \item For each \code{value}, keeps only the row with the highest
#'     \code{from_surv_id} (the most recent applicable label).
#' }
#' This ensures the function always returns the latest label for each answer
#' value that was valid at the time of \code{the_surv_id}.
#'
#' @examples
#' \dontrun{
#' answers <- SVsurvey_read_answers("EB2023_1")
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter slice_max select
#'
#' @export
SVsurvey_read_answers <- function(the_surv_id) {

   file <- "03_process_editions/Metadata/Answers.xlsx"

   if ( !file.exists(file) ) stop("File ",file," does not exist!")

   answers <- readxl::read_xlsx(file)

   missing_columns <-
      c("value","from_surv_id","vallabel") |>
      setdiff(names(answers))

   if ( length(missing_columns)>0 ) {
      missing_columns <- paste(missing_columns,collapse=", ")
      stop("Following columns are missing in ",file," but are required: ",missing_columns)
      }

   answers <- answers |>
      dplyr::filter(from_surv_id<=the_surv_id) |>
      dplyr::slice_max(from_surv_id,by=value) |>
      dplyr::select(-from_surv_id)

   return(answers)

}
