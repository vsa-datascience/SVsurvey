
# -------------------------------------------------------------------------
# DATA STRUCTURE DEFINITIONS ----------------------------------------------
# -------------------------------------------------------------------------


rm(list=ls())

library(tidyverse)

folder <- r"{\\WV162699\fs_kb_dkb\svr\4_Beveiligde_data\SV-bevragingen\01_Metadata}"




# Create file with new names ----------------------------------------------

# tmp1 <- file.path(folder,"variabelen.xlsx") |>
#    readxl::read_xlsx() |>
#    filter_out(is.na(varname)) |>
#    select(varname,varlabel,question,subquestion,Opmerkingen,`Opmerkingen intern`) |>
#    write_csv("99_code_for_transfer/tmp/data_for_variables.csv")

# prompt:
# The attached csv file is a list of variables in a dataset. These are the columns:
# + varname: the name of the variable. it is often derived from the dutch description. if underscores are used, it usually refers to a suffix that is common across a question battery.
# + varlabel: a variable label, description of the variable
# + question: The question that is asked in a survey
# + subquestion: The item of a question that is given in a survey
# + Opmerkingen: some additional information
# + Opmerkingen intern: some other additional information
# Please make a new dataset that proposes new varnames, descriptions, and labels. It should contain the following columns:
# + varname: the varname from the original file
# + ID: a new varname, but using english rather than dutch as the language. Try to give items from a question battery the same prefix. Also try to use consistency in naming.
# + description: a description in english of the content of the variable in english
# + label_nl: a short label for the variabel that can be used in graphs and tables in dutch
# + label_fr: a short label for the variabel that can be used in graphs and tables in french
# + label_en: a short label for the variabel that can be used in graphs and tables in english
# Please provide this as a csv file.
# Could you create the dataset but try to limit the ID to 8 characters, or if that’s really not possible, a maximum of 10 more or less (use varname column as an example)? Could you also write it out in UTF-8?
# Also check that the ID is mainly aligned with English terminology (e.g., rreg refers to rijksregister, instead of nr refering to national register).





# create concept list -----------------------------------------------------

tmp1 <- "99_code_for_transfer/input/recode_variables.xlsx" |>
   readxl::read_xlsx()

tmp2 <- "99_code_for_transfer/input/recode_code_lists.xlsx" |>
   readxl::read_xlsx() |>
   select(old_scale,scale) |>
   distinct() |>
   filter_out(is.na(old_scale))


tmp3 <- file.path(folder,"variabelen.xlsx") |>
   readxl::read_xlsx() |>
   filter_out(is.na(varname)) |>
   left_join(tmp1,join_by(varname==old_varname)) |>
   left_join(tmp2,join_by(scale==old_scale))

tmp4 <- tmp3 |>
   mutate(
      measure=replace_values(measure,"string"~"character","nominal"~"codelist","ordinal"~"codelist"),
      measure=str_c("format=",measure),
      weetniet=ifelse(weetniet=='x' & measure=="format=codelist",'dkn',NA),
      filter=ifelse((!is.na(filter)) & measure=="format=codelist" ,"na",NA),
      invalid=ifelse((!is.na(question)) & measure=="format=codelist","inv",NA),
      codelist=pmap_chr(list(scale.y,weetniet,filter,invalid),function(...){str_flatten(c(...),collapse="_",na.rm=TRUE)}),
      codelist=ifelse(codelist=="",NA,str_c("codelist=",codelist)),
      format=ifelse(is.na(format),NA,str_c("regex=",format)),
      fmt=pmap_chr(list(measure,format,codelist),function(...){str_flatten(c(...),collapse=";",na.rm=TRUE)}),
      # fmt=str_c(measure,"(",fmtargs,")"),
      format_SVsurvey_sample="",
      format_SVsurvey_fieldworkdata=ifelse(bron=="enquete",fmt,""),
      format_SVsurvey_cleandata=fmt,
      format_SVsurvey_SUF=ifelse(ScientificUseFile=='x',fmt,""),
      format_SVsurvey_PUF=ifelse(PublicUseFile=='x',fmt,""),
      ) |>
   select(
      variable=new_varname,description_nl,label_nl,label_fr,label_en,
      label_SVsurvey_questionmodule=vragenlijstmodule,label_SVsurvey_question=question,label_SVsurvey_questionitem=subquestion,
      format_SVsurvey_sample,format_SVsurvey_fieldworkdata,format_SVsurvey_cleandata,format_SVsurvey_SUF,format_SVsurvey_PUF,
      remarks=`Opmerkingen intern`,
      # matches("^SV\\d{4}$")
      )

# writexl::write_xlsx(tmp4,r"{02_create_data_structure_definitions\input\dsd_variables.xlsx}")


tmp <- r"{..\02_sourcedata\SV0001\01_sample\SAMP_VSA_SV_THEMATIQUE.xlsx}" |>
   readxl::read_xlsx() |>
   names() |>
   enframe(value="variable_sampledata",name=NULL) |>
   mutate(variable="") |>
   select(variable,variable_sampledata)
# writexl::write_xlsx(tmp1,r"{02_create_data_structure_definitions\input\dsd_SV0001_sample_skeleton.xlsx}")








