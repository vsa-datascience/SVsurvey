
SVsurvey_labelled_2_tibble <- function(labelled_data, the_surv_id, ...) {

   # Input check --------------------------------------------------------------
   if (!is.data.frame(labelled_data)) {
      stop("`labelled_data` must be a data.frame or tibble.")
      }

   # Temporarily widen output options for readable error messages -------------
   old <- options(warning.length = 8170, width = 10000)
   on.exit(options(old), add = TRUE)

   # Variable label check -----------------------------------------------------

   varlabels <- dplyr::left_join(
      SVsurvey_read_survey_variables(the_surv_id),
      SVsurvey_read_questions(the_surv_id),
      dplyr::join_by(concept_id)
      )

   varlabels_data <- labelled::get_variable_labels(labelled_data) |>
         tibble::enframe(name = "variable",value = "value") |>
         tidyr::unnest(value)

   new_varlabels <- c(...)

   if (!is.null(new_varlabels)) {

      # Check that all names are unique in new_varlabels
      dup_vars <- duplicated(names(new_varlabels))
      if ( any(dup_vars) ) {
         dup_vars <- names(new_varlabels)[dup_vars] |> unique() |> paste(collapse=", ")
         stop("Some variables occurs serveral times in the variable labels correction: ",dup_vars)
         }

      # Check that all names in new_varlabels exist in varlabels
      missing_vars <- setdiff(names(new_varlabels), varlabels_data$variable)
      if (length(missing_vars) > 0) {
         stop("Label corrections defined for Variables that are not in Questions.xlsx: ",
                 paste(missing_vars, collapse = ", "))
         }

      varlabels_data <- coalesce_join(
         varlabels_data,
         tibble::enframe(new_varlabels,name = "variable",value = "value"),
         dplyr::join_by(variable)
         )

      }

   tmp1 <- varlabels_data |>
      dplyr::left_join(varlabels, dplyr::join_by(variable)) |>
      dplyr::filter(!is.na(value)) |>       # only check requested vars
      dplyr::filter(value != varlabel)         # keep mismatches only

   if ( nrow(tmp1)>0 ) {
      errors <- tmp1 |>
         dplyr::mutate(
            error = ifelse(
               dplyr::row_number()!=dplyr::n(),
               sprintf('\n\t"%s"="%s", # instead of "%s"',variable, varlabel, value),
               sprintf('\n\t"%s"="%s"  # instead of "%s"',variable, varlabel, value)
               )
         ) |>
         dplyr::pull(error)
      stop("Wrong variable labels, check and change with:", errors)
      }


   # Convert labelled data to tibble ------------------------------------------
   tibbled_data <- labelled_data |>
      dplyr::mutate(
         dplyr::across(where(labelled::is.labelled), labelled::to_character),
         dplyr::across(where(is.character),          stringr::str_squish),
         dplyr::across(where(is.factor),\(x) forcats::fct_relabel(x, stringr::str_squish))
         ) |>
      haven::zap_formats() |>
      haven::zap_label()   |>
      haven::zap_widths()

   return(tibbled_data)
}