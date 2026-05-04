the_surv_id <- "SV0003"

tmp1 <- r"{..\02_sourcedata\%s\02_survey}" |>
   sprintf(the_surv_id) |>
   file.path("SV3 - Datafile - finaal.sav") |>
   haven::read_sav() |>
   names() |>
   tibble::enframe(name=NULL,value="variable") |>
   dplyr::mutate(variable_lc=stringr::str_to_lower(variable))

tmp2 <- r"{\\WV162699\fs_kb_dkb\svr\4_Beveiligde_data\SV-bevragingen\01_Metadata\variabelen.xlsx}" |>
   readxl::read_xlsx() |>
   dplyr::select(old_varname=varname,variable_lc={{the_surv_id}}) |>
   dplyr::filter_out(is.na(variable_lc))

tmp3 <- r"{XX_code_for_transfer\input\recode_variables.xlsx}" |>
   readxl::read_xlsx() |>
   dplyr::select(old_varname,concept_id=new_varname)

tmp4 <- tmp1 |>
   dplyr::left_join(tmp2,dplyr::join_by(variable_lc)) |>
   dplyr::left_join(tmp3,dplyr::join_by(old_varname)) |>
   dplyr::select(variable,concept_id) |>
   dplyr::mutate(concept_id=dplyr::replace_values(concept_id,"resp_disp"~"resp_fwdisp"))

sprintf(r"{03_process_editions\%s\input\%s_variables_survey--created.xlsx}",the_surv_id,the_surv_id) |>
   writexl::write_xlsx(x=tmp4)
