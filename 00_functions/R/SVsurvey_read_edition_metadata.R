#' Read Survey Edition Metadata
#'
#' Reads metadata for a specific survey edition from the editions Excel file.
#' Validates that the file exists, contains all required columns, and that the
#' requested survey ID exists exactly once in the metadata.
#'
#' @param the_surv_id A character string specifying the survey ID to retrieve
#'   metadata for. Must match exactly one row in the editions file.
#'
#' @return A single-row \code{tibble} containing the metadata for the requested
#'   survey edition, with at minimum the following columns:
#'   \describe{
#'     \item{surv_id}{Survey identifier}
#'     \item{fw_period}{Fieldwork period}
#'     \item{surv_year}{Survey year}
#'     \item{surv_semester}{Survey semester}
#'     \item{sample_n}{Sample size}
#'   }
#'
#' @details
#' The function reads from a fixed path:
#' \code{03_process_editions/Metadata/Editions.xlsx}.
#' It performs three validation checks before returning data:
#' \enumerate{
#'   \item The Excel file must exist at the expected path.
#'   \item The file must contain all five required columns.
#'   \item The requested \code{the_surv_id} must appear exactly once.
#' }
#'
#' @examples
#' \dontrun{
#' metadata <- SVsurvey_read_edition_metadata("SV0001")
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter
#'
#' @export
SVsurvey_read_edition_metadata <- function(the_surv_id) {

   file <- "03_process_editions/Metadata/Editions.xlsx"

   if ( !file.exists(file) ) stop("File ",file," does not exist!")

   edition <- readxl::read_xlsx(file)

   missing_variables <-
      c("surv_id","fw_period","surv_year","surv_semester","sample_n") |>
      setdiff(names(edition))

   if ( length(missing_variables)>0 ) {
      missing_variables <- paste(missing_variables,collapse=", ")
      stop("Following columns are missing in ",file," but are required: ",missing_variables)
      }

   edition <- dplyr::filter(edition,surv_id==the_surv_id)

   if ( nrow(edition)==0 ) {
      stop(
         "Edition ",the_surv_id,
         " has not yet been added to ",file,". ",
         "Please, add the required information."
         )
      }

   if ( nrow(edition)>1 ) {
      stop(
         "Edition ",the_surv_id,
         " occurs multiple times in file ",file,". ",
         "Please, reduce to one row."
         )
      }

   return(edition)

}