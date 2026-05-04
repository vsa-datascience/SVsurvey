rm(list = ls())
# Clear the entire workspace: remove all objects currently stored in memory.
# This helps avoid accidentally using old variables from previous runs.

library(tidyverse)
# Load a collection of packages for data handling:
# dplyr (filter/mutate/join), tidyr (pivot/nest/unnest), purrr (map/walk),
# stringr (string helpers), tibble (modern data frames), etc.

inputfolder <- "01_data_model/01_code_lists/03_country_codes/input"
# This is the folder where we will load the input files from.
outputfolder <- "../03_deriveddata/01_codelists"
# This is the folder where we will write the generated Excel files (one per codelist).





# worldregion -----------------------------------------------------

worldregion <- inputfolder |>
   file.path("worldregion.xlsx") |>
   readxl::read_xlsx()

outputfolder |>
  file.path('cl_worldregion.xlsx') |>
  writexl::write_xlsx(x=worldregion)





# belgium -----------------------------------------------------------------

belgium <- inputfolder |>
   file.path("belgium.xlsx") |>
   readxl::read_xlsx()

outputfolder |>
  file.path('cl_belgium.xlsx') |>
  writexl::write_xlsx(x=belgium)





# countrycat1 -------------------------------------------------------------

countrycat1 <- inputfolder |>
   file.path("countrycat1.xlsx") |>
   readxl::read_xlsx() |>
   rename(cl_belgium=belgium)

outputfolder |>
  file.path('cl_countrycat1.xlsx') |>
  writexl::write_xlsx(x=countrycat1)





# countrycat2 -------------------------------------------------------------

countrycat2 <- inputfolder |>
   file.path("countrycat2.xlsx") |>
   readxl::read_xlsx() |>
   rename(cl_countrycat1=countrycat1) |>
   left_join(select(countrycat1,value,cl_belgium),join_by(cl_countrycat1==value))

outputfolder |>
  file.path('cl_countrycat2.xlsx') |>
  writexl::write_xlsx(x=countrycat2)




# ISO-codes countries -----------------------------------------------------

tmp <- inputfolder |>
   file.path("countryISO.xlsx") |>
   readxl::read_xlsx() |>
   rename(cl_worldregion=worldregion,cl_countrycat2=countrycat2) |>
   full_join(select(countrycat2,-valuen,-starts_with('label_')),join_by(cl_countrycat2==value)) |>
   relocate(valuen)

countryISO2 <- tmp |>
  rename(value=value2,cl_countryISO3=value3) |>
  relocate(cl_countryISO3, .after = last_col())

outputfolder |>
  file.path('cl_countryISO2.xlsx') |>
  writexl::write_xlsx(x=countryISO2)

countryISO3 <- tmp |>
  rename(value=value3,cl_countryISO2=value2) |>
  relocate(cl_countryISO2, .after = last_col())

outputfolder |>
  file.path('cl_countryISO3.xlsx') |>
  writexl::write_xlsx(x=countryISO3)

rm(tmp)




# NIS-codes countries -----------------------------------------------------

countryNIS <- inputfolder |>
  file.path("countrynis.xlsx") |>
  readxl::read_xlsx() |>
  rename(cl_countryISO3=countryISO3) |>
  mutate(value=as.character(valuen),.after=1) |>
  arrange(valuen) |>
  separate_longer_delim(cl_countryISO3,';') |>
  left_join(
    select(countryISO3,-valuen,-starts_with('label_')),
    join_by(cl_countryISO3==value)
    ) |>
  summarize(across(everything(),~.x |> unique() |> str_c(collapse=';')),.by=c(valuen,value) ) |>
  mutate(
    cl_countrycat2 = ifelse(valuen>=700 & is.na(cl_countrycat2),"other"     ,cl_countrycat2),
    cl_countrycat1 = ifelse(valuen>=700 & is.na(cl_countrycat1),"notEU27"   ,cl_countrycat1),
    cl_belgium     = ifelse(valuen>=700 & is.na(cl_belgium)    ,"notBelgium",cl_belgium),
    )

outputfolder |>
  file.path('cl_countryNIS.xlsx') |>
  writexl::write_xlsx(x=countryNIS)





# NIS-codes countries for nationality -------------------------------------

countryNIS_natlty <- inputfolder |>
  file.path("countrynis_natlty.xlsx") |>
  readxl::read_xlsx() |>
  rename(cl_countryISO3=countryISO3) |>
  mutate(value=as.character(valuen),.after=1) |>
  arrange(valuen) |>
  separate_longer_delim(cl_countryISO3,';') |>
  left_join(
    select(countryISO3,-valuen,-starts_with('label_')),
    join_by(cl_countryISO3==value)
    ) |>
  summarize(across(everything(),~.x |> unique() |> str_c(collapse=';')),.by=c(valuen,value) ) |>
  mutate(
    cl_countrycat2 = ifelse(valuen>=8700 & is.na(cl_countrycat2),"other"     ,cl_countrycat2),
    cl_countrycat1 = ifelse(valuen>=8700 & is.na(cl_countrycat1),"notEU27"   ,cl_countrycat1),
    cl_belgium     = ifelse(valuen>=8700 & is.na(cl_belgium)    ,"notBelgium",cl_belgium),
    )

outputfolder |>
  file.path('cl_countryNISnatlty.xlsx') |>
  writexl::write_xlsx(x=countryNIS_natlty)

















