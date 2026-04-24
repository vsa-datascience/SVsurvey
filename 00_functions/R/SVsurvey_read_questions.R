#' Read Survey Questions Metadata
#'
#' Reads question metadata from the shared \code{Questions.xlsx} file and
#' returns the most recent version of each question concept that applies to
#' the requested survey edition.
#'
#' @param the_surv_id A character string specifying the survey ID. Used to
#'   filter questions to those introduced at or before this edition.
#'
#' @return A \code{tibble} with one row per concept, containing the most recent
#'   question wording applicable to \code{the_surv_id}. Columns include at
#'   minimum:
#'   \describe{
#'     \item{concept_id}{Concept identifier}
#'     \item{varlabel}{Variable label}
#'     \item{question_module}{Thematic module the question belongs to}
#'     \item{question}{Full question text}
#'     \item{question_item}{Specific item within the question (for grid/battery questions)}
#'   }
#'   Note: \code{from_surv_id} is dropped from the returned tibble.
#'
#' @details
#' The function reads from the fixed path
#' \code{03_process_editions/Metadata/Questions.xlsx}, which is a
#' longitudinal registry of question wordings. Each row represents a version
#' of a question concept introduced from a given survey edition onwards
#' (\code{from_surv_id}).
#'
#' The filtering logic applies two steps:
#' \enumerate{
#'   \item Retains only rows where \code{from_surv_id <= the_surv_id},
#'     i.e. question versions that were already in use by this edition.
#'   \item For each \code{concept_id}, keeps only the row with the highest
#'     \code{from_surv_id} (the most recent applicable wording).
#' }
#' This means the function always returns the latest wording of each question
#' that was valid at the time of \code{the_surv_id}.
#'
#' @examples
#' \dontrun{
#' questions <- SVsurvey_read_questions("EB2023_1")
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter slice_max select
#'
#' @export
SVsurvey_read_questions <- function(the_surv_id) {

   file <- "03_process_editions/Metadata/Questions.xlsx"

   if ( !file.exists(file) ) stop("File ",file," does not exist!")

   questions <- readxl::read_xlsx(file)

   missing_columns <-
      c("concept_id","from_surv_id","varlabel","question_module","question","question_item") |>
      setdiff(names(questions))

   if ( length(missing_columns)>0 ) {
      missing_columns <- paste(missing_columns,collapse=", ")
      stop("Following columns are missing in ",file," but are required: ",missing_columns)
      }

   questions <- questions |>
      dplyr::filter(from_surv_id<=the_surv_id) |>
      dplyr::slice_max(from_surv_id,by=concept_id) |>
      dplyr::select(-from_surv_id)

   return(questions)

}
