#' Create an Excel codebook for an SV survey edition
#'
#' This function generates an Excel codebook (`.xlsx`) for a given SV survey
#' edition. The codebook combines general survey metadata, variable information,
#' question texts, and answer codes/labels based on the survey DSD and associated
#' codelists.
#'
#' The function:
#' \itemize{
#'   \item retrieves general survey metadata from
#'   \code{02_process_editions/Metadata/Editions.xlsx};
#'   \item reads the survey DSD from the derived data folder;
#'   \item resolves referenced codelists and expands answer codes and labels;
#'   \item formats all information into a styled Excel worksheet named
#'   \code{"Codebook"}; and
#'   \item writes the result to the \code{03_deriveddata/03_Codebooks} directory.
#' }
#'
#' The function stops with an error if the survey edition is missing or occurs
#' more than once in the Editions metadata file, or if a referenced codelist
#' cannot be found.
#'
#' @param the_surv_id Character string identifying the survey edition
#'   (e.g. \code{"SV0001"}). Must match exactly one row in
#'   \code{Editions.xlsx} and correspond to an existing survey DSD file.
#'
#' @return This function is called for its side effects. It returns
#'   \code{NULL} invisibly and writes an Excel codebook to disk.
#'
#' @details
#' The output file is named \code{Codebook_<the_surv_id>.xlsx}.
#' Existing files with the same name are overwritten.
#'
#' All file paths are assumed to be relative to the project root and follow
#' the standard SV survey data production folder structure.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[openxlsx]{createWorkbook}}
#'   \item \code{\link[readxl]{read_xlsx}}
#' }
#'
#' @export
SVsurvey_create_codebook <- function(the_surv_id) {

   # CREATE XLSX WORKSHEET ---------------------------------------------------

   wb <- openxlsx::createWorkbook()
   openxlsx::addWorksheet(wb=wb,sheetName='Codebook')
   row=1


   # GENERAL INFORMATION -----------------------------------------------------

   tmp_metainfo <-
      "02_process_editions/Metadata/Editions.xlsx" |>
      readxl::read_xlsx() |>
      dplyr::filter(surv_id==the_surv_id)

   if ( nrow(tmp_metainfo)==0 ) {
      stop(
         "Edition ",the_surv_id,
         " has not yet been added to '02_process_editions/Metadata/Editions.xlsx'. ",
         "Please, add the required information."
         )
      }

   if ( nrow(tmp_metainfo)>1 ) {
      stop(
         "Edition ",the_surv_id,
         " occurs multiple times in the file '02_process_editions/Metadata/Editions.xlsx'. ",
         "Please, reduce to one row."
         )
      }

   tmp_info <- tibble::tribble(
      ~key,~value,
      'TITEL',paste0('Codeboek SV-bevraging ',the_surv_id,' (',tmp_metainfo$surv_semester,' ',tmp_metainfo$surv_year,')'),
      'BESCHRIJVING',paste0('Bevraging voor update Vlaamse Openbare Statistieken bij ',tmp_metainfo$sample_n,' Vlamingen'),
      'CONTACTPERSOON',tmp_metainfo$cntctprs,
      'VELDWERKBUREAU',tmp_metainfo$fw_org,
      )
   openxlsx::writeData(wb,'Codebook',tmp_info,startRow=row,colNames=FALSE)
   row=row+nrow(tmp_info)+1


   # HEADER ------------------------------------------------------------------

   openxlsx::addStyle(
      wb,
      'Codebook',
      openxlsx::createStyle(fgFill='#4B6C7A',fontColour='#FFFFFF'),
      rows=row,
      cols=1:5,
      gridExpand = TRUE
      )
   tmp_header <- tibble::tribble(
      ~varname    ,~varlabel,~vraag  ,~valn          ,~valc          ,
      'Variabele' ,'Label'  ,'Vraag' ,'Antwoordcode' ,'Antwoordlabel',
      )
   openxlsx::writeData(wb,'Codebook',tmp_header,startRow=row,colNames=FALSE)
   row=row+nrow(tmp_header)


   # INHOUDELIJKE VARIABELEN -------------------------------------------------

   tmp_dsd <-
      "../03_deriveddata/02_DSD/02_surveydata/dsd_%s_surveydata.xlsx" |>
      sprintf(the_surv_id) |>
      readxl::read_xlsx() |>
      tidyr::separate_longer_delim(format,";") |>
      dplyr::mutate(format=stringr::str_squish(format)) |>
      tidyr::separate_wider_delim(format,"=",names=c("name","value")) |>
      tidyr::pivot_wider() |>
      dplyr::mutate(
         codelist=purrr::map(
            codelist,
            \(x) if(is.na(x)){NULL} else{ dplyr::select(load_codelist(x),valuen,vallabel=label_SVsurvey) }
            )
         ) |>
      tidyr::unnest(codelist,keep_empty=TRUE) |>
      tidyr::unite(question,label_SVsurvey_question,label_SVsurvey_questionitem,sep=" ",na.rm=TRUE) |>
      dplyr::mutate(
         varname=ifelse(dplyr::row_number()==1,concept_id,""),
         varlabel=ifelse(dplyr::row_number()==1,label_SVsurvey,""),
         question=ifelse(dplyr::row_number()==1,question,""),
         valn=dplyr::case_when(
            valuen %% 1 == 0 ~ sprintf("%.0f",valuen),
            valuen %% 1 != 0 ~ sprintf("%.2f",valuen),
            TRUE ~ paste0("[",format,"]")
            ),
         valc=vallabel,
         .by=concept_id
         ) |>
      dplyr::select(varname,varlabel,question,valn,valc)

   openxlsx::writeData(wb,'Codebook',tmp_dsd,startRow=row,colNames=FALSE)
   row=row+nrow(tmp_dsd)


   # WRITE XLSX WORKSHEET ----------------------------------------------------

   openxlsx::addStyle(
      wb,
      'Codebook',
      openxlsx::createStyle(halign = 'left'),
      rows=1:row,
      cols=1:4,
      gridExpand=TRUE,
      stack=TRUE
      )
   openxlsx::setColWidths(
      wb,
      'Codebook',
      1:5,
      c(20,50,30,20,30)
      )
   filename <- "../03_deriveddata/03_Codebooks/Codebook_%s.xlsx" |>
      sprintf(the_surv_id)
   openxlsx::saveWorkbook(wb,filename,overwrite=TRUE)

}