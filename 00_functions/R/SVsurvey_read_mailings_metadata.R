#' Read Mailings Metadata
#'
#' Reads mailing dates for a specific fieldwork period from the shared
#' \code{mailings.xlsx} metadata file and returns them in wide format, with
#' one column per contact type.
#'
#' @param the_fw_period A character string specifying the fieldwork period to
#'   retrieve mailing dates for (e.g. \code{"SV0002-SV0003"}). Must match exactly one
#'   set of mailing rows in the metadata file.
#'
#' @return A single-row \code{tibble} with one column per contact type:
#'   \describe{
#'     \item{fw_period}{Fieldwork period identifier}
#'     \item{invitation}{Date of the invitation mailing}
#'     \item{reminder_1}{Date of the first reminder mailing}
#'     \item{reminder_2}{Date of the second reminder mailing}
#'   }
#'
#' @details
#' The function reads from the fixed path
#' \code{03_process_editions/Metadata/mailings.xlsx}. It validates that the
#' file exists and contains the required columns (\code{fw_period},
#' \code{contact}, \code{date}).
#'
#' After filtering to \code{the_fw_period}, the function checks that each of
#' the three expected contact types (\code{invitation}, \code{reminder_1},
#' \code{reminder_2}) appears exactly once. An informative error is thrown if
#' any contact type is missing or duplicated.
#'
#' The validated long-format data is then pivoted to wide format using
#' \code{tidyr::pivot_wider()}, producing one date column per contact type.
#'
#' @examples
#' \dontrun{
#' mailings <- SVsurvey_read_mailings_metadata("2023_1")
#' }
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter count mutate pull
#' @importFrom tidyr complete pivot_wider
#' @importFrom forcats fct_relevel
#'
#' @export
SVsurvey_read_mailings_metadata <- function(the_fw_period) {

   file <- "03_process_editions/Metadata/mailings.xlsx"

   if ( !file.exists(file) ) stop("File ",file," does not exist!")

   mailings <- readxl::read_xlsx(file)

   missing_variables <-
      c("fw_period","contact","date") |>
      setdiff(names(mailings))

   if ( length(missing_variables)>0 ) {
      missing_variables <- paste(missing_variables,collapse=", ")
      stop("Following columns are missing in ",file," but are required: ",missing_variables)
      }

   the_contacts <- c("invitation","reminder_1","reminder_2")

   mailings <- mailings |>
      dplyr::filter(fw_period==the_fw_period) |>
      dplyr::filter(contact %in% the_contacts)

   tmp <- mailings |>
      dplyr::count(contact) |>
      dplyr::mutate(contact=factor(contact,the_contacts)) |>
      tidyr::complete(contact, fill = list(n = 0)) |>
      dplyr::filter_out(n==1)

   if ( nrow(tmp)>0 ) {
      tmp <- tmp |>
         dplyr::mutate(
            message=ifelse(n==0,
               sprintf("line is missing for %s.",contact),
               sprintf("too many lines for %s.",contact)
               )
            ) |>
         dplyr::pull(message) |>
         paste(collapse="\n")
      stop(
         "\nFile ",file," not right for field work period ",the_fw_period,".\n",
         tmp
         )
      }

   mailings <- mailings |>
      tidyr::pivot_wider(names_from="contact",values_from="date")

   return(mailings)

}