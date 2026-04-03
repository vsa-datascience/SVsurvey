rm(list = ls())
# Clear the entire workspace: remove all objects currently stored in memory.
# This helps avoid accidentally using old variables from previous runs.

library(tidyverse)
# Load a collection of packages for data handling:
# dplyr (filter/mutate/join), tidyr (pivot/nest/unnest), purrr (map/walk),
# stringr (string helpers), tibble (modern data frames), etc.


# -------------------------------------------------------------------------
# Output folder
# -------------------------------------------------------------------------

outputfolder <- "../03_deriveddata/01_codelists"
# This is the folder where we will write the generated Excel files (one per codelist).

unlink(outputfolder, recursive = TRUE)
# Delete the folder if it already exists, including any files inside it.
# recursive = TRUE means: also delete everything inside the folder.

dir.create(outputfolder)
# Create a fresh empty folder so the results can be written cleanly.


# -------------------------------------------------------------------------
# Source all codes
# -------------------------------------------------------------------------

source(r"{01_create_code_lists\01_general_codelists\create_general_code_lists.R}")
source(r"{01_create_code_lists\02_NIS_codes\create_code_lists_NIScodes.R}")
source(r"{01_create_code_lists\03_country_codes\create_code_lists_countries.R}")