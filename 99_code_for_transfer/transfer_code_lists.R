rm(list=ls())

library(tidyverse)

folder <- r"{\\WV162699\fs_kb_dkb\svr\4_Beveiligde_data\SV-bevragingen\01_Metadata}"




# -------------------------------------------------------------------------
# CODE LISTS --------------------------------------------------------------
# -------------------------------------------------------------------------



# create file with new names ----------------------------------------------

tmp1 <- file.path(folder,"schalen.xlsx") |>
   readxl::read_xlsx() |>
   filter_out(is.na(scale)) |>
   write_csv("99_code_for_transfer/tmp/data_for_values.csv")

# prompt:
# The attached csv file is a list of values for codelists of categorical variables in a dataset. These are the columns:
# + scale: the name of the codelist. it is often derived from the dutch description.
# + valn: the numeric value of the category
# + valc: the character value of the category, in dutch
# Please make a new dataset that proposes new ID's, descriptions, and labels. It should contain the following columns:
# + scale: the scale from the original file
# + valn: the numeric value of the category from the original file
# + scale_ID: a new ID based on the scale, that can be used as a short universal code while programming. Base the id on the content of column valc. Try using english rather than dutch as the language. Try to limit the ID to 8 characters, or if that’s really not possible, a maximum of 10 more or less. It should follow standard naming conventions for variables in datasets (e.g. no spaces, start with letter, all small caps)
# + value_ID: a new ID based on the valc, that can be used as a short universal code while programming. Try using english rather than dutch as the language. Try to limit the ID to 8 characters, or if that’s really not possible, a maximum of 10 more or less. It should follow standard naming conventions for variables in datasets (e.g. no spaces, start with letter, all small caps)
# + description_nl: a description in dutch of the content of the category
# + label_nl: a short label for the category that can be used in graphs and tables in dutch
# + label_fr: a short label for the category that can be used in graphs and tables in french
# + label_en: a short label for the category that can be used in graphs and tables in english
# Please provide this as a csv file. Could you write it out in UTF-8?







# tmp1 <- "99_code_for_transfer/input/recode_code_lists.xlsx" |>
#    readxl::read_xlsx() |>
#    mutate(across(starts_with("label_"),str_to_lower)) |>
#    writexl::write_xlsx("99_code_for_transfer/input/recode_code_lists.xlsx")



tmp1 <- "99_code_for_transfer/input/recode_code_lists.xlsx" |>
   readxl::read_xlsx()

# # check tmp1
# tmp <- tmp1 |>
#    distinct(value,description_nl,label_nl,label_fr,label_en) |>
#    mutate(n=n(),.by=value) |>
#    filter(n>1) |>
#    arrange(value)


tmp2 <- tmp1 |> select(old_scale,new_scale=scale) |> distinct()

tmp3 <- file.path(folder,"variabelen.xlsx") |>
   readxl::read_xlsx() |>
   filter_out(is.na(scale)) |>
   mutate(
      weetniet=ifelse(weetniet=='x','dkn',NA),
      filter=ifelse(is.na(filter),NA,"na"),
      invalid=ifelse(is.na(question),NA,"inv"),
      ) |>
   select(scale,weetniet,filter,invalid) |>
   distinct() |>
   left_join(tmp2,join_by(scale==old_scale)) |>
   arrange(scale,weetniet,filter)

tmp4 <- tmp3 |>
   filter(invalid=="inv") |>
   select(scale,new_scale) |>
   bind_rows(tmp3) |>
   distinct()

tmp5 <- tmp4 |>
   mutate(
      name=pmap_chr(list(new_scale,weetniet,filter,invalid),function(a,b,c,d){str_flatten(c(a,b,c,d),collapse="_",na.rm=TRUE)})
      ) |>
   select(-scale) |>
   mutate(overall_scale=new_scale) |>
   pivot_longer(new_scale|weetniet|filter|invalid,names_to=NULL,values_to="scale",values_drop_na=TRUE) |>
   left_join(select(tmp1,scale,valuen,value),join_by(scale),relationship = "many-to-many") |>
   mutate(indicator=TRUE) |>
   nest(.by=overall_scale) |>
   mutate(data=map(data,~.x |> select(-scale) |> pivot_wider(names_from="name",values_from="indicator") |> mutate(across(where(is.logical),~replace_na(.x,FALSE)))))

tmp5 |>
   arrange(overall_scale) |>
   pull(data,overall_scale) |>
   writexl::write_xlsx("99_code_for_transfer/tmp/code_lists_skeletons.xlsx")


tmp6 <- tmp1 |>
   select(value,description_nl,starts_with("label")) |>
   distinct()

# check unique value
tmp6 |> count(value) |> filter(n>1) |> View()


tmp6 |>
   writexl::write_xlsx("99_code_for_transfer/tmp/code_lists_values.xlsx")














# Add label_SVsurvey ------------------------------------------------------

tmp1 <- "99_code_for_transfer/input/recode_code_lists.xlsx" |>
   readxl::read_xlsx() |>
   select(scale=old_scale,valn=valuen,value) |>
   filter_out(scale=="missing")


tmp2 <- r"{\\WV162699\fs_kb_dkb\svr\4_Beveiligde_data\SV-bevragingen\01_Metadata}" |>
   file.path("schalen.xlsx") |>
   readxl::read_xlsx() |>
   filter_out(is.na(scale)) |>
   rename(label_SVsurvey=valc) |>
   mutate(valn=as.numeric(valn)) |>
   left_join(tmp1,join_by(scale,valn)) |>
   select(value,label_SVsurvey) |>
   distinct() |>
   filter_out(label_SVsurvey=="Andere situatie")

tmp3 <- r"{01_create_code_lists/input/code_lists_values.xlsx}" |>
   readxl::read_xlsx() |>
   left_join(tmp2,join_by(value))


# writexl::write_xlsx(tmp3,r"{01_create_code_lists/input/code_lists_values.xlsx}")









