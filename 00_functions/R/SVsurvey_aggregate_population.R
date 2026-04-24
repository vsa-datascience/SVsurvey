#' Aggregate population distributions for SV survey weighting
#'
#' This function derives aggregated population distributions from Demobel stock
#' data for use in SV survey weighting and calibration. It computes relative
#' population shares by key covariates such as sex, age group, nationality group,
#' and municipality grouping.
#'
#' The function:
#' \itemize{
#'   \item reads Demobel stock data for the specified year;
#'   \item selects the appropriate NIS reference version valid for that year;
#'   \item restricts the population to adults (18+);
#'   \item derives analytical variables (sex, age categories, nationality group);
#'   \item aggregates the population by each covariate separately; and
#'   \item writes the resulting distributions to an Excel file.
#' }
#'
#' Each output sheet contains relative frequencies (summing to 1) for one
#' covariate and can be used directly as population margins in survey calibration
#' procedures.
#'
#' @param year Integer year for which the population distribution should be
#'   computed (e.g. \code{2023}). This year is used to select both the Demobel
#'   stock data and the applicable NIS reference version.
#'
#' @return This function is called for its side effects. It returns
#'   \code{NULL} invisibly and writes an Excel file with aggregated population
#'   distributions to disk.
#'
#' @details
#' The function assumes the presence of:
#' \itemize{
#'   \item Demobel stock data file
#'     \code{02_sourcedata/Demobel-stock/stock_<year>.sav};
#'   \item NIS reference codelists named
#'     \code{cl_refnisYYYYlvl4.xlsx};
#'   \item the nationality codelist \code{cl_countryNIS}.
#' }
#'
#' The most recent NIS reference version not exceeding \code{year} is used.
#' Existing output files are overwritten without warning.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{SVsurvey_load_codelist}}
#'   \item \code{\link[haven]{read_sav}}
#'   \item \code{\link[writexl]{write_xlsx}}
#' }
#'
#' @export
SVsurvey_aggregate_population <- function(year) {

   # creëer bestaandnamen op basis van locatie
   inputfile  <- paste0("../02_sourcedata/Demobel-stock/stock_",year,".sav")
   outputfile <- paste0("../03_deriveddata/03_aggregated_population_data/stock_",year,".xlsx")

   refnis_version <- "../03_deriveddata/01_codelists" |>
      list.files() |>
      stringr::str_subset("^cl_refnis[0-9]{4}lvl4[.]xlsx$") |>
      stringr::str_replace("^cl_refnis([0-9]{4})lvl4[.]xlsx$","\\1") |>
      as.numeric() |>
      purrr::keep(\(x) x<=year) |>
      max()

   cl_refnislvl4 <- refnis_version |>
      sprintf(fmt="cl_refnis%ilvl4") |>
      SVsurvey_load_codelist() |>
      dplyr::filter( !!rlang::sym(sprintf("cl_refnis%ilvl1",refnis_version))=="02000") |>
      dplyr::select(value,matches(sprintf("^cl_refnis%ilvl2$",refnis_version))) |>
      dplyr::rename_with(\(x) stringr::str_remove(x,"^cl_")) |>
      dplyr::mutate(value=as.numeric(value))

   cl_countryNIS <- SVsurvey_load_codelist("cl_countryNIS") |>
      dplyr::select(valuen,natltygrp=cl_countrycat1)

   # lees data in
   data_full <- inputfile |>
     haven::read_sav(col_select=c(CD_REFNIS,CD_SEX,MS_AGE,CD_NATLTY)) |>
      dplyr::filter(CD_REFNIS %in% cl_refnislvl4$value) |>
      dplyr::filter(MS_AGE>=18) |>
      dplyr::mutate(
         sex=dplyr::recode_values(CD_SEX,1~'male',2~'female'),
         agecat7=age2agecat7from18(MS_AGE),
         ) |>
      dplyr::left_join(cl_countryNIS,dplyr::join_by(CD_NATLTY==valuen)) |>
      dplyr::left_join(cl_refnislvl4,dplyr::join_by(CD_REFNIS==value)) |>
      dplyr::select(sex,agecat7,natltygrp,matches(sprintf("^refnis%ilvl2$",refnis_version)))

   # functie om een variabele te aggregeren
   aggregate_by_covariate <- function(.covar) {
      data_full |>
      dplyr::filter_out(is.na(!!rlang::sym(.covar))) |>
      dplyr::summarize(p=dplyr::n(),.by=tidyselect::all_of(.covar)) |>
      dplyr::mutate(p=p/sum(p))
      }

   # aggregeer per variabele
   data_agg <- data_full |>
      names() |>
      purrr::map(aggregate_by_covariate) |>
      rlang::set_names(names(data_full))

   # schrijf weg
   writexl::write_xlsx(data_agg,outputfile)

}