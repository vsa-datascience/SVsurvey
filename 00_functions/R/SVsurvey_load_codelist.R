#' Load an SV survey codelist from Excel
#'
#' This function reads a codelist used in SV survey production from the
#' standard codelist directory and returns it as a tibble.
#'
#' The codelist is expected to be stored as an Excel file named
#' \code{<cl_name>.xlsx} in the \code{03_deriveddata/01_codelists} directory.
#' The function stops with an error if the requested file does not exist.
#'
#' @param cl_name Character string giving the name of the codelist
#'   (without file extension), e.g. \code{"GESLACHT"}.
#'
#' @return A tibble containing the contents of the codelist Excel file.
#'
#' @details
#' File paths are assumed to be relative to the project root and follow
#' the standard SV survey data production folder structure.
#'
#' @seealso
#' \code{\link[readxl]{read_xlsx}}
#'
#' @export
SVsurvey_load_codelist <- function(cl_name) {
   file <- paste0("../03_deriveddata/01_codelists/",cl_name,".xlsx")
   if (!file.exists(file)){
      stop("Code list ",file," does not exist!")
      }
   return( readxl::read_xlsx(file) )
   }