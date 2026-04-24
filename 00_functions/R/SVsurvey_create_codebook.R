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

   tmp_editions <- SVsurvey_read_edition_metadata(the_surv_id)

   tmp_info <- tibble::tribble(
      ~key,~value,
      'TITEL',paste0('Codeboek SV-bevraging ',the_surv_id,' (',tmp_editions$surv_semester,' ',tmp_editions$surv_year,')'),
      'BESCHRIJVING',paste0('Bevraging voor update Vlaamse Openbare Statistieken bij ',tmp_editions$sample_n,' Vlamingen'),
      'CONTACTPERSOON',tmp_editions$cntctprs,
      'VELDWERKBUREAU',tmp_editions$fw_org,
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

   tmp_variables <- SVsurvey_read_survey_variables(the_surv_id)
   tmp_questions <- SVsurvey_read_questions(the_surv_id)
   tmp_answers   <- SVsurvey_read_answers(the_surv_id)
   tmp_dsd       <- SVsurvey_load_dsd(tmp_variables$concept_id)

   tmp_error <- setdiff(tmp_variables$concept_id,tmp_questions$concept_id)
   if ( length(tmp_error)>0 ) {
      tmp_error <- paste(tmp_error,collapse=", ")
      stop(
         "\nThe following concept_id's are not in 03_process_editions/Metadata/Questions.xlsx: ",
         tmp_error,
         "\nPlease, correct the variable list or update the overview of questions."
         )
      }

   tmp_variables_2 <- tmp_variables |>
      dplyr::left_join(tmp_dsd,dplyr::join_by(concept_id)) |>
      dplyr::left_join(tmp_questions,dplyr::join_by(concept_id)) |>
      dplyr::mutate(
         constraint_codelist=purrr::map(
            constraint_codelist,
            \(x) dplyr::select(x,any_of(c("valuen","value")))
            )
         ) |>
      tidyr::unnest(constraint_codelist,keep_empty=TRUE)

   tmp_error <- setdiff(tmp_variables_2$value,tmp_answers$value) |>
      na.omit()
   if ( length(tmp_error)>0 ) {
      tmp_error <- paste(tmp_error,collapse=", ")
      stop(
         "\nThe following caluess are in the codelists of ",
         "01_variables_",the_surv_id,".xlsx ",
         "but not in Answers.xlsx: ",
         tmp_error,
         "\nPlease, change the variable list or update the data model."
         )
      }

   tmp_variables_3 <- tmp_variables_2 |>
      dplyr::left_join(tmp_answers,dplyr::join_by(value)) |>
      tidyr::unite(question,question,question_item,sep=" ",na.rm=TRUE) |>
      dplyr::mutate(
         varname=ifelse(dplyr::row_number()==1,variable,""),
         varlabel=ifelse(dplyr::row_number()==1,varlabel,""),
         question=ifelse(dplyr::row_number()==1,question,""),
         valn=dplyr::case_when(
            valuen %% 1 == 0 ~ sprintf("%.0f",valuen),
            valuen %% 1 != 0 ~ sprintf("%.2f",valuen),
            TRUE ~ paste0("[",constraint_type,"]")
            ),
         valc=vallabel,
         .by=concept_id
         ) |>
      dplyr::select(varname,varlabel,question,valn,valc)

   openxlsx::writeData(wb,'Codebook',tmp_variables_3,startRow=row,colNames=FALSE)
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
   filename <- "../03_deriveddata/04_Codebooks/Codebook_%s.xlsx" |>
      sprintf(the_surv_id)
   openxlsx::saveWorkbook(wb,filename,overwrite=TRUE)

   # WRITE XLSX WORKSHEET ----------------------------------------------------
   message("Code book ",the_surv_id," written!")

}
