rm(list=ls())

library(tidyverse)
devtools::load_all("00_functions")


# Load dsd ----------------------------------------------------------------

dsd <- '01_data_model/02_DSD/input_dsd.xlsx' |>
  readxl::read_xlsx()


# Load all code list names ------------------------------------------------

codelists <- "../03_deriveddata/01_codelists" |>
   list.files() |>
   str_remove("[.]xlsx$")


# CHECKS ------------------------------------------------------------------

errors <- c()

### check whether each concept ID is unique
tmp <- dsd |>
  filter(duplicated(concept_id)) |>
  mutate(error=sprintf('concept_id "%s" is not unique',concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether types are right
tmp <- dsd |>
  filter(!constraint_type %in% c("character","date","datetime","codelist","integer","numeric")) |>
  mutate(error=sprintf("Type '%s' for concept_id '%s' is not correct. Must be 'character', 'date', 'datetime', 'codelist', 'integer' or 'numeric'.",constraint_type,concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether code list only appears with code list concepts
tmp <- dsd |>
  filter_out(is.na(constraint_codelist)) |>
  filter_out(constraint_type=='codelist') |>
  mutate(error=sprintf("Concept '%s' has codelist but is not codelist type",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check if there is always a code list for code list concepts
tmp <- dsd |>
  filter(constraint_type=='codelist') |>
  filter(is.na(constraint_codelist)) |>
  mutate(error=sprintf("Concept '%s' does not have a code list.",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether code lists exist
tmp <- dsd |>
  filter_out(is.na(constraint_codelist)) |>
  mutate(test=map_lgl(constraint_codelist,\(x) x %in% codelists)) |>
  filter_out(test) |>
  mutate(error=sprintf("Code list for concept '%s' does not exist: '%s'",concept_id,constraint_codelist)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether regex only appears with character concepts
tmp <- dsd |>
  filter_out(is.na(constraint_regex)) |>
  filter_out(constraint_type=='character') |>
  mutate(error=sprintf("Concept '%s' has regex but is not type haracter.",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether regex's are valid
tmp <- dsd |>
  filter_out(is.na(constraint_regex)) |>
  mutate(test=map_lgl(constraint_regex,is_valid_regex)) |>
  filter_out(test) |>
  mutate(error=sprintf("Value for 'regex' for concept '%s' is not a valid regex: '%s'",concept_id,constraint_regex)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether min and max are numeric
if (!is.numeric(dsd$constraint_min)) errors <- c(errors,"Column constraint_min is not numeric")
if (!is.numeric(dsd$constraint_max)) errors <- c(errors,"Column constraint_max is not numeric")

### check whether min only appears with integer/numeric concepts
tmp <- dsd |>
  filter_out(is.na(constraint_min)) |>
  filter_out(constraint_type %in% c('integer','numeric')) |>
  mutate(error=sprintf("Concept '%s' has minimum value but is not type integer/numeric",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether max only appears with integer/numeric concepts
tmp <- dsd |>
  filter_out(is.na(constraint_max)) |>
  filter_out(constraint_type %in% c('integer','numeric')) |>
  mutate(error=sprintf("Concept '%s' has minimum value but is not type integer/numeric",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether min is smaller than max
tmp <- dsd |>
  filter_out(is.na(constraint_min)) |>
  filter_out(is.na(constraint_max)) |>
  filter_out(constraint_min<=constraint_max) |>
  mutate(error=sprintf("Concept '%s' has constraint_min larger than constraint_max",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether constraint_allow_missings is logical
if (!is.logical(dsd$constraint_allow_missings)) errors <- c(errors,"Column constraint_allow_missings is not logical")

### output errors

if ( length(errors>0) ) {
  errors <- str_c(errors,collapse='\n')
  stop('\n',errors)
  }


# PUBLISH -----------------------------------------------------------------

writexl::write_xlsx(dsd,"../03_deriveddata/02_DSD/dsd.xlsx")











