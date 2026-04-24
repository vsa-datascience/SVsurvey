#' Read Survey Variables
#'
#' Reads the variables definition file for a specific survey edition from the
#' corresponding input Excel file.
#'
#' @param the_surv_id A character string specifying the survey ID to retrieve
#'   variables for. Used to construct the path to the input file.
#'
#' @return A \code{tibble} containing the survey variables, with at minimum the
#'   following columns:
#'   \describe{
#'     \item{concept_id}{Concept identifier linking the variable to a concept}
#'     \item{variable}{Variable name used by the fieldwork agency}
#'     \item{variable_label}{Human-readable label for the variable, used by the fieldwork agency}
#'   }
#'
#' @details
#' The function reads from a path constructed from \code{the_surv_id}:
#' \code{03_process_editions/<surv_id>/input/<surv_id>_variables_survey.xlsx}.
#' It validates that the file exists and contains all three required columns
#' before returning the data.
#'
#' @examples
#' \dontrun{
#' variables <- SVsurvey_read_survey_variables("EB2023_1")
#' }
#'
#' @importFrom readxl read_xlsx
#'
#' @export
SVsurvey_read_survey_variables <- function(the_surv_id) {

   file <-
      "03_process_editions/%s/input/%s_variables_survey.xlsx" |>
      sprintf(the_surv_id,the_surv_id)

   if ( !file.exists(file) ) stop("File ",file," does not exist!")

   variables <- readxl::read_xlsx(file)

   missing_columns <-
      c("concept_id","variable","variable_label") |>
      setdiff(names(variables))

   if ( length(missing_columns)>0 ) {
      missing_columns <- paste(missing_columns,collapse=", ")
      stop("Following columns are missing in ",file," but are required: ",missing_columns)
      }

   return(variables)

}