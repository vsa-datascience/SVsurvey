rm(list=ls())

library(tidyverse)

create_dsd_SVsurvey_input <- function(surv_id) {

   # Load DSD variables

   dsd_variables <- r"{02_create_data_structure_definitions\dsd_variables.xlsx}" |>
      readxl::read_xlsx()

   # DSD sample data

   dsd_sample_data <-
      r"{02_create_data_structure_definitions\%s\dsd_%s_skeleton.xlsx}" |>
      sprintf(surv_id,surv_id) |>
      readxl::read_xlsx(sheet="sample") |>
      left_join(dsd_variables,join_by(concept_id)) |>
      select(concept_id=variable,label_nl,label_fr,label_en,format=format_SVsurvey_sample)

   r"{..\03_deriveddata\02_DSD\01_samples\dsd_%s_sample.xlsx}" |>
      sprintf(surv_id) |>
      writexl::write_xlsx(x=dsd_sample_data)

   # DSD survey data

   dsd_survey_data <-
      r"{02_create_data_structure_definitions\%s\dsd_%s_skeleton.xlsx}" |>
      sprintf(surv_id,surv_id) |>
      readxl::read_xlsx(sheet="surveydata") |>
      left_join(dsd_variables,join_by(concept_id)) |>
      select(
         concept_id=variable,
         label_nl,label_fr,label_en,
         label_SVsurvey=variable_label,
         format=format_SVsurvey_surveydata
         )

   r"{..\03_deriveddata\02_DSD\02_surveydata\dsd_%s_surveydata.xlsx}" |>
      sprintf(surv_id) |>
      writexl::write_xlsx(x=dsd_survey_data)
   }

create_dsd_SVsurvey_input('SV0001')