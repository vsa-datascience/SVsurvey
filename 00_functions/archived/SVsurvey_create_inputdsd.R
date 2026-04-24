#' Create input DSD files for an SV survey edition
#'
#' This function generates the derived DSD input files for sample data and
#' survey data for a given SV survey edition. Starting from the survey-specific
#' DSD skeleton and the central DSD variable definitions, it produces harmonised
#' Excel DSD files that are ready for downstream use (e.g. codebook generation,
#' validation, or processing scripts).
#'
#' Specifically, the function:
#' \itemize{
#'   \item loads the central DSD variable definitions;
#'   \item reads the edition-specific DSD skeleton file;
#'   \item enriches sample and survey data structures with labels and formats;
#'   \item writes the resulting DSDs to the standard derived data folders.
#' }
#'
#' Two files are created:
#' \itemize{
#'   \item a sample DSD in \code{02_DSD/01_samples};
#'   \item a survey data DSD in \code{02_DSD/02_surveydata}.
#' }
#'
#' @param the_surv_id Character string identifying the survey edition
#'   (e.g. \code{"SV0001"}). This ID is used to locate the edition-specific
#'   DSD skeleton and to name the output files.
#'
#' @return This function is called for its side effects. It returns
#'   \code{NULL} invisibly and writes two Excel DSD files to disk.
#'
#' @details
#' The function assumes the following inputs exist:
#' \itemize{
#'   \item \code{01_data_model/02_DSD/dsd_variables.xlsx};
#'   \item \code{02_process_editions/<the_surv_id>/dsd_<the_surv_id>_skeleton.xlsx},
#'         with sheets \code{"sample"} and \code{"surveydata"}.
#' }
#'
#' Existing output files are overwritten without warning.
#' All file paths are assumed to be relative to the project root and follow
#' the standard SV survey data production folder structure.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[readxl]{read_xlsx}}
#'   \item \code{\link[writexl]{write_xlsx}}
#' }
#'
#' @export
SVsurvey_create_inputdsd <- function(the_surv_id) {

   # Load DSD variables

   dsd_variables <- "01_data_model/02_DSD/dsd_variables.xlsx" |>
      readxl::read_xlsx()

   # DSD sample data

   dsd_sample_data <-
      r"{03_process_editions/%s/01_dsd_%s_skeleton.xlsx}" |>
      sprintf(the_surv_id,the_surv_id) |>
      readxl::read_xlsx(sheet="sample") |>
      dplyr::left_join(dsd_variables,dplyr::join_by(concept_id)) |>
      dplyr::select(concept_id=variable,label_nl,label_fr,label_en,format=format_SVsurvey_sample)

   "../03_deriveddata/02_DSD/01_samples/dsd_%s_sample.xlsx" |>
      sprintf(the_surv_id) |>
      writexl::write_xlsx(x=dsd_sample_data)

   # DSD survey data

   dsd_survey_data <-
      r"{03_process_editions/%s/01_dsd_%s_skeleton.xlsx}" |>
      sprintf(the_surv_id,the_surv_id) |>
      readxl::read_xlsx(sheet="surveydata") |>
      dplyr::left_join(dsd_variables,dplyr::join_by(concept_id)) |>
      dplyr::select(
         concept_id=variable,
         label_nl,label_fr,label_en,
         format=format_SVsurvey_surveydata
         )

   "../03_deriveddata/02_DSD/02_surveydata/dsd_%s_surveydata.xlsx" |>
      sprintf(the_surv_id) |>
      writexl::write_xlsx(x=dsd_survey_data)

   }