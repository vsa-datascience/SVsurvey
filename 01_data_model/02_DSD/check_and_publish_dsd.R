rm(list=ls())

library(tidyverse)
devtools::load_all("00_functions")

outputpath <- '../03_deriveddata/02_DSD'



# -------------------------------------------------------------------------
# Load dsd and codelists --------------------------------------------------
# -------------------------------------------------------------------------


dsd <- '01_data_model/02_DSD/dsd_variables.xlsx' |>
  readxl::read_xlsx()


# -------------------------------------------------------------------------
# Load all code list names ------------------------------------------------
# -------------------------------------------------------------------------

codelists <- "../03_deriveddata/01_codelists" |>
   list.files() |>
   str_remove("[.]xlsx$")



# -------------------------------------------------------------------------
# CHECKS ------------------------------------------------------------------
# -------------------------------------------------------------------------

errors <- c()


# Conceptlist checks ------------------------------------------------------

### check whether each concept ID is unique
tmp <- dsd |>
  filter(duplicated(concept_id)) |>
  mutate(error=sprintf('concept_id "%s" is not unique',concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether formats are right
tmp <- dsd |>
  filter(!format %in% c("character","date","codelist","integer","numeric")) |>
  mutate(error=sprintf("Format '%s' for concept_id '%s' is not correct. Must be 'character', 'date', 'codelist', 'integer' or 'numeric'.",format,concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether codelist only appears with codelist concepts
tmp <- dsd |>
  filter_out(is.na(codelist)) |>
  filter_out(format=='codelist') |>
  mutate(error=sprintf("Concept '%s' has codelist but is not codelist format",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check if there is always a code list for codelist concepts
tmp <- dsd |>
  filter(format=='codelist') |>
  filter(is.na(codelist)) |>
  mutate(error=sprintf("Concept '%s' does not have a code list.",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether codelists exist
tmp <- dsd |>
  filter_out(is.na(codelist)) |>
  mutate(test=map_lgl(codelist,\(x) x %in% codelists)) |>
  filter_out(test) |>
  mutate(error=sprintf("Code list for concept '%s' does not exist: '%s'",concept_id,codelist)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether regex only appears with character concepts
tmp <- dsd |>
  filter_out(is.na(regex)) |>
  filter_out(format=='character') |>
  mutate(error=sprintf("Concept '%s' has regex but is not character.",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether regex's are valid
tmp <- dsd |>
  filter_out(is.na(regex)) |>
  mutate(test=map_lgl(regex,is_valid_regex)) |>
  filter_out(test) |>
  mutate(error=sprintf("Value for 'regex' for concept '%s' is not a valid regex: '%s'",concept_id,regex)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether min and max are numeric
if (!is.numeric(dsd$min)) errors <- c(errors,"Column min is not numeric")
if (!is.numeric(dsd$max)) errors <- c(errors,"Column max is not numeric")

### check whether min only appears with integer/numeric concepts
tmp <- dsd |>
  filter_out(is.na(min)) |>
  filter_out(format %in% c('integer','numeric')) |>
  mutate(error=sprintf("Concept '%s' has minimum value but is not integer/numeric",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether max only appears with integer/numeric concepts
tmp <- dsd |>
  filter_out(is.na(max)) |>
  filter_out(format %in% c('integer','numeric')) |>
  mutate(error=sprintf("Concept '%s' has minimum value but is not integer/numeric",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether min is smaller than max
tmp <- dsd |>
  filter_out(is.na(min)) |>
  filter_out(is.na(max)) |>
  filter_out(min<=max) |>
  mutate(error=sprintf("Concept '%s' has minimum value larger than maximum value",concept_id)) |>
  pull(error)
errors <- c(errors,tmp)
rm(tmp)

### check whether missing_values and complete are logical
if (!is.logical(dsd$missing_values)) errors <- c(errors,"Column missing_values is not logical")
if (!is.logical(dsd$complete)) errors <- c(errors,"Column complete is not logical")

### output errors

if ( length(errors>0) ) {
  errors <- str_c(errors,collapse='\n')
  stop('\n',errors)
  }


# clean up ----------------------------------------------------------------

rm(errors)




# -------------------------------------------------------------------------
# PUBLISH -----------------------------------------------------------------
# -------------------------------------------------------------------------

writexl::write_xlsx(dsd,"../03_deriveddata/02_DSD/dsd.xlsx")











