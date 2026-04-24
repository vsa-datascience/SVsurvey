#' Load Data Structure Definition (DSD)
#'
#' Reads the Data Structure Definition from a shared Excel file and returns
#' constraint metadata for a specified set of concept IDs, with any referenced
#' codelists loaded and nested as tibbles.
#'
#' @param the_concept_ids An optional character vector of concept IDs to
#'   retrieve from the DSD. If \code{NULL} (the default), all concepts in the
#'   DSD are returned. If provided, all supplied IDs must be present in the
#'   file or an error is thrown.
#'
#' @return A \code{tibble} containing at minimum the following columns:
#'   \describe{
#'     \item{concept_id}{Concept identifier}
#'     \item{label_nl}{Dutch-language label for the concept}
#'     \item{constraint_type}{Type of constraint applied to the variable}
#'     \item{constraint_codelist}{A list-column where each element is either
#'       an empty \code{tibble} (if no codelist applies) or a \code{tibble}
#'       returned by \code{SVsurvey_load_codelist()}}
#'     \item{constraint_regex}{Regular expression constraint, if applicable}
#'     \item{constraint_min}{Minimum value constraint, if applicable}
#'     \item{constraint_max}{Maximum value constraint, if applicable}
#'     \item{constraint_allow_missings}{Whether missing values are permitted}
#'   }
#'   Filtered to \code{the_concept_ids} if provided, otherwise the full DSD.
#'
#' @details
#' The function reads from the fixed path
#' \code{../03_deriveddata/02_DSD/dsd.xlsx}. It performs three validation
#' checks before returning data:
#' \enumerate{
#'   \item The Excel file must exist at the expected path.
#'   \item The file must contain all eight required columns.
#'   \item If \code{the_concept_ids} is not \code{NULL}, every supplied concept
#'     ID must be present in the DSD — no silent dropping of requested concepts.
#' }
#'
#' After filtering, the \code{constraint_codelist} column is mutated into a
#' list-column by calling \code{SVsurvey_load_codelist()} on each non-\code{NA}
#' entry, allowing downstream code to access codelist tibbles directly without
#' additional loading steps.
#'
#' @seealso \code{\link{SVsurvey_load_codelist}}
#'
#' @examples
#' \dontrun{
#' # Load specific concepts
#' dsd <- SVsurvey_load_dsd(c("q1_satisfaction", "q2_trust"))
#'
#' # Load the full DSD
#' dsd <- SVsurvey_load_dsd()
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter mutate
#' @importFrom purrr map
#'
#' @export
SVsurvey_load_dsd <- function(the_concept_ids=NULL) {

   file <- "../03_deriveddata/02_DSD/dsd.xlsx"

   if (!file.exists(file)){
      stop("Code list ",file," does not exist!")
      }

   dsd <- readxl::read_xlsx(file)

   missing_columns <-
      c("concept_id","label_nl","constraint_type","constraint_codelist","constraint_regex","constraint_min","constraint_max","constraint_allow_missings") |>
      setdiff(names(dsd))

   if ( length(missing_columns)>0 ) {
      missing_columns <- paste(missing_columns,collapse=", ")
      stop("Following columns are missing in ",file," but are required: ",missing_columns)
      }

   missing_concepts <- the_concept_ids |>
      setdiff(dsd$concept_id)

   if ( length(missing_concepts)>0 ) {
      missing_concepts <- paste(missing_concepts,collapse=", ")
      stop(
         "\nFollowing concepts are missing in the DSD: ",missing_concepts,
         "\nPlease update DSD (or correct own files).")
      }

   if ( !is.null(the_concept_ids) ) {
      dsd <- filter(dsd,concept_id %in% the_concept_ids)
      }

   dsd <- dsd |>
      dplyr::mutate(
         constraint_codelist=purrr::map(
            constraint_codelist,
            \(x) if(is.na(x)){
               tibble(value=character())
               } else {
               SVsurvey_load_codelist(x) |>
               dplyr::select(any_of("valuen"),value,starts_with("label"))
               }
            )
         )

   return(dsd)

}

