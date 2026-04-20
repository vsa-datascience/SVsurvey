rm(list = ls())
# Clear the entire workspace: remove all objects currently stored in memory.
# This helps avoid accidentally using old variables from previous runs.

library(tidyverse)
# Load a collection of packages for data handling:
# dplyr (filter/mutate/join), tidyr (pivot/nest/unnest), purrr (map/walk),
# stringr (string helpers), tibble (modern data frames), etc.


inputfolder <- "01_data_model/01_code_lists/01_general_codelists/input"
# This is the folder where we will load the input files from.
outputfolder <- "../03_deriveddata/01_codelists"
# This is the folder where we will write the generated Excel files (one per codelist).


# -------------------------------------------------------------------------
# Load the "value list" (master list with metadata)
# -------------------------------------------------------------------------

values <- inputfolder |>
   file.path("code_lists_values.xlsx") |>
  readxl::read_xlsx()
# Read in the master Excel file containing metadata about values.
# Example columns:
#   - value: the actual code (e.g., "YES", "NO", "DK")
#   - valuen: an ordering number
#   - description, label_nl, etc.
#
# The pipe operator |> means:
#   "Take the left side and pass it as the first argument into the function on the right."


# -------------------------------------------------------------------------
# Load the code list skeletons (which values belong to which codelist)
# -------------------------------------------------------------------------

read_xlsx_to_list <- function(path) {
  # This helper function reads an Excel file where each sheet is a separate table.
  # It returns a named list:
  #   - each list element is one sheet as a tibble
  #   - the element name is the sheet name

  path |>
    readxl::excel_sheets() |>
    set_names() |>
    map(~ readxl::read_xlsx(path, sheet = .x))
  # excel_sheets(path) gives the sheet names
  # set_names() uses those sheet names as list names
  # map(...) reads each sheet and returns a list of tibbles
}

skeletons <- inputfolder |>
   file.path("code_lists_skeletons.xlsx") |>
   read_xlsx_to_list()
# Read the skeleton workbook into a named list of tibbles.
# Example: skeletons[["yesno"]] is the tibble from sheet "yesno".

rm(read_xlsx_to_list)
# Remove the helper function from memory (optional cleanup).


# -------------------------------------------------------------------------
# Validate: ensure skeleton values exist in the master value list
# -------------------------------------------------------------------------

check <- skeletons |>
  enframe(name = "skeleton_name", value = "skeleton") |>
  # enframe() converts a list into a tibble:
  #   - skeleton_name = list item names (sheet names)
  #   - skeleton = the tibble stored in each list element

  mutate(skeleton = map(skeleton, ~ select(.x, value))) |>
  # For each skeleton table, keep only the column "value"
  # (so validation focuses only on whether the code exists)

  unnest(skeleton) |>
  # Convert the nested tibbles into one big long tibble
  # (one row per value per skeleton)

  anti_join(values, join_by(value))
  # anti_join keeps rows from the left side that do NOT match the right side.
  # Here: values used in skeletons that are missing from the master `values` list.

if (nrow(check) > 0) {
  # If there are any missing values, stop the script.

  check <- check |>
    mutate(
      code = sprintf("'%s' in skeleton '%s'", value, skeleton_name),
      .keep = "none"
      # .keep = "none" drops other columns, so we keep only "code"
    ) |>
    pull(code) |>
    # pull(code) turns the code column into a plain character vector

    str_c(collapse = "; ")
    # Join all missing items into one readable message

  stop(
    "Values not in value list: ",
    check,
    "\nCheck the code list skeletons, or update the code list values."
  )
  # stop() immediately stops the script and prints the message
}

rm(check)
# Remove temporary validation object


# -------------------------------------------------------------------------
# CREATE SEPARATE CODELISTS -----------------------------------------------
# -------------------------------------------------------------------------

process_skeleton <- function(skeleton) {
  # This function takes ONE skeleton sheet/table and:
  # 1) figures out which values belong to which codelists
  # 2) joins metadata from the master list `values`
  # 3) creates mapping columns for each codelist
  # 4) writes the resulting codelists as Excel files into outputfolder

  test <- skeleton |>

    pivot_longer(
      -matches("^valuen?$"),
      names_to = "codelist",
      values_to = "filter"
    ) |>
    # pivot_longer makes the data "long":
    # - Keep columns valuen and/or value (because we exclude them with -matches(...))
    # - All other columns become:
    #     codelist = column name (e.g., "yesno", "gender", ...)
    #     filter   = the cell value (often TRUE/FALSE)
    #
    # matches("^valuen?$") matches:
    #  - "value"
    #  - "valuen"
    # so we keep those and pivot everything else.

    mutate(codelist = str_c("cl_", codelist)) |>
    # Prepend "cl_" to codelist names (naming convention)

    nest(.by = c(valuen, value)) |>
    # Group rows by (valuen, value) and nest the remaining columns into a list-column.
    # Result: each unique (valuen, value) becomes one row with a small tibble in `data`.

    mutate(
      data = map(
        data,
        \(d) cross_join(
          d,
          rename_with(d, \(n) str_c(n, "_map"))
        )
      )
    ) |>
    # For each nested tibble `d`, we create a "cross join" of:
    #   - d itself
    #   - a renamed copy of d where column names get suffix "_map"
    #
    # A cross join makes all combinations of rows.
    # This is typically used to compare every codelist to every mapping version.
    #
    # rename_with(d, \(n) str_c(n, "_map")) renames columns:
    #   e.g. codelist -> codelist_map, filter -> filter_map

    unnest(data) |>
    # Flatten the nested data back into a regular tibble

    mutate(
      value_map = ifelse(
        all(filter | !filter_map) & filter_map,
        value,
        NA
      ),
      .by = c(codelist, codelist_map)
    ) |>
    # Create a new column `value_map` (a mapping result).
    #
    # ifelse(condition, yes, no):
    #   - if condition is TRUE -> keep `value`
    #   - else -> NA
    #
    # .by = c(codelist, codelist_map) means:
    # "Evaluate the all(...) condition within each (codelist, codelist_map) group."

    filter(filter) |>
    # Keep only rows where filter is TRUE
    # (i.e., keep only values that belong to the codelist)

    left_join(values, join_by(value)) |>
    # Add metadata columns (labels, description...) from the master list.

    select(-starts_with("filter")) |>
    # Remove helper columns like filter and filter_map

    nest(.by = codelist) |>
    # Create one nested tibble per codelist

    pull(data, codelist) |>
    # Convert tibble into a named list:
    # - values are the nested tibbles
    # - names are the codelist values

    map(\(d) pivot_wider(d, names_from = "codelist_map", values_from = "value_map")) |>
    # Convert mapping information back into "wide" format:
    # each mapping target becomes a column

    map(\(d) select(d, where(\(var) !all(is.na(var))))) |>
    # Drop columns that are entirely NA

    imap(\(d, l) select(d, -all_of(l))) |>
    # imap iterates over list elements and their names.
    # Here: remove the column whose name equals the list element name `l`
    # (often the "self-mapping" column you don't want to keep)

    imap(\(d, l) writexl::write_xlsx(d, str_c(outputfolder, "/", l, ".xlsx")))
    # Write each tibble `d` to an Excel file.
    # File name is based on the codelist name `l`.
}

walk(skeletons, process_skeleton)
# walk() applies a function to each element of a list, like map(),
# but it is used when you mainly care about side effects (here: writing files)
# rather than returning a new object.































