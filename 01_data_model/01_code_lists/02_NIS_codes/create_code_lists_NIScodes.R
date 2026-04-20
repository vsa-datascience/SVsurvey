rm(list = ls())
# Clear the entire workspace: remove all objects currently stored in memory.
# This helps avoid accidentally using old variables from previous runs.

library(tidyverse)
# Load a collection of packages for data handling:
# dplyr (filter/mutate/join), tidyr (pivot/nest/unnest), purrr (map/walk),
# stringr (string helpers), tibble (modern data frames), etc.

inputfolder <- "01_data_model/01_code_lists/02_NIS_codes/input"
# This is the folder where we will load the input files from.
outputfolder <- "../03_deriveddata/01_codelists"
# This is the folder where we will write the generated Excel files (one per codelist).





# STEP 1: Make hierarchical files per version -----------------------------
# -------------------------------------------------------------------------

create_hierarchical_data <- function(data,version) {
  for (lvl in 0:4) {
    tmpid = NA
    tmpname=str_c(version,'lvl',lvl)
    for (i in 1:nrow(data)) {
      tmplvl = data[['level']][i]
      if ( tmplvl < lvl ) {
        tmpid = NA
        } else if ( tmplvl == lvl ) {
        tmpid = data[i,'id']
        }
      data[i,tmpname]=tmpid
      if ( tmplvl > lvl & !is.na(tmpid) ) {
        data[i,'parent_id']=tmpid
        }
      }
    }
    return(data)
  }


for ( version in c(1995,2019,2025) ) {
  tmp_inputfile  <- str_c(inputfolder,'/REFNIS',version,'.xlsx')
  tmp_inputfile6 <- str_c(inputfolder,'/REFNIS_6_',version,'.xlsx')
  tmp_outputobject <- str_c('refnis',version)
  tmp1 <- tmp_inputfile |>
    readxl::read_xlsx(col_types='text') |>
    mutate(level=parse_integer(level)) |>
    create_hierarchical_data(version)
  tmp2 <- tmp1 |>
    filter(level==4) |>
    select(starts_with(str_c(version,'lvl'))) |>
    mutate(parent_id=!!sym(str_c(version,'lvl4')))
  tmp3 <- tmp_inputfile6 |>
    readxl::read_xlsx(col_types='text') |>
    filter(!is.na(id)) |>
    left_join(tmp2,join_by(parent_id)) |>
    mutate(
      level=5,
      !!str_c(version,'lvl5'):=id,
      across(starts_with('label'),str_to_title)
    )
  tmp4 <- tmp1 |>
    bind_rows(tmp3) |>
    relocate(id,level,label_nl,label_fr,parent_id,num_range(str_c(version,'lvl'),0:5)) |>
    arrange(!!!syms(str_c(version,'lvl',0:5))) |>
    rename_with(~str_c('NIS',.x),all_of(str_c(version,'lvl',0:5))) |>
    mutate(label_en=label_fr,.after='label_fr') |>
    assign(x=tmp_outputobject)
  rm(list=ls(pattern="^tmp"))
  }

rm(create_hierarchical_data)





# STEP 2: Add mapping 2025 ------------------------------------------------
# -------------------------------------------------------------------------

refnis2025_withmaps <- refnis2025





# STEP 3: Add mapping 2019 ------------------------------------------------
# -------------------------------------------------------------------------

### level 4 mappings

tmp_recodes <- str_c(inputfolder,'/recodes_2025.xlsx') |>
  readxl::read_xlsx(col_types='text') |>
  separate_longer_delim(`Fuserende gemeenten`,'+') |>
  mutate(
    NIS2019lvl4 = str_extract(`Fuserende gemeenten`,'[0-9]{5}'),
    NIS2025lvl4 = str_extract(`Nieuwe gemeenten`,'[0-9]{5}'),
    .keep='none'
  )

tmp_lvl4_2025 <- refnis2025 |>
  filter(level=='4') |>
  select(num_range('NIS2025lvl',0:4))

tmp_lvl4 <- refnis2019 |>
  filter(level=='4') |>
  left_join(tmp_recodes,join_by(id==NIS2019lvl4)) |>
  mutate(NIS2025lvl4 = coalesce(NIS2025lvl4,id)) |>
  left_join(tmp_lvl4_2025,join_by(NIS2025lvl4)) |>
  mutate(NIS2025=NIS2025lvl4)


### mapping level 0 to 3

tmp_2019to2025_lvl0to3 <- tmp_lvl4 |>
  select(id,matches('^NIS[0-9]{4}lvl[0-3]')) |>
  pivot_longer(
    matches('^NIS[0-9]{4}lvl[0-3]'),
    names_to=c('version','maplevel'),
    names_sep='lvl',
    values_to='mapid',
    values_drop_na=TRUE,
  ) |>
  arrange(version,maplevel,mapid,id) |>
  summarize(ids=str_c(id,collapse=';'),.by=c(version,maplevel,mapid)) |>
  pivot_wider(names_from='version',values_from='mapid') |>
  filter(!is.na(NIS2019)) |>
  select(-maplevel,-ids)

tmp_lvl0to3 <- refnis2019 |>
  filter(level %in% as.character(0:3)) |>
  pivot_longer(
    matches('^NIS2019lvl'),
    names_to=c(NA,'maplevel'),
    names_sep='lvl',
    values_to ='NIS2019'
  ) |>
  left_join(tmp_2019to2025_lvl0to3,join_by(NIS2019)) |>
  pivot_wider(
    names_from=maplevel,
    names_sep='lvl',
    values_from=c('NIS2019','NIS2025')
  ) |>
  left_join(tmp_2019to2025_lvl0to3,join_by(id==NIS2019))

### level 5

tmp_2019lvl4_2025 <- tmp_lvl4 |>
  select(NIS2019lvl4,starts_with('NIS2025lvl'))

tmp_lvl5 <- refnis2019 |>
  filter(level=='5') |>
  left_join(tmp_2019lvl4_2025,join_by(NIS2019lvl4)) |>
  mutate(
    NIS2025lvl5=NIS2019lvl5,
    NIS2025    =NIS2019lvl5
  )

### bind together

refnis2019_withmaps <-
  bind_rows(tmp_lvl0to3,tmp_lvl4,tmp_lvl5) |>
  arrange(level,NIS2019lvl0,NIS2019lvl1,NIS2019lvl2,NIS2019lvl3,NIS2019lvl4,NIS2019lvl5)

rm(list=ls(pattern="^tmp"))








# STEP 4: add mapping 1995 ------------------------------------------------
# -------------------------------------------------------------------------



### level 4 mappings

tmp_recodes <- str_c(inputfolder,'/recodes_2019.xlsx') |>
  readxl::read_xlsx(col_types='text') |>
  mutate(
    NIS1995lvl4 = `Oude code`,
    NIS2019lvl4 = `Nieuwe code`,
    .keep='none'
  )

tmp_lvl4_2019 <- refnis2019 |>
  filter(level=='4') |>
  select(num_range('NIS2019lvl',0:4))

tmp_lvl4 <- refnis1995 |>
  filter(level=='4') |>
  left_join(tmp_recodes,join_by(id==NIS1995lvl4)) |>
  mutate(NIS2019lvl4 = coalesce(NIS2019lvl4,id)) |>
  left_join(tmp_lvl4_2019,join_by(NIS2019lvl4)) |>
  mutate(NIS2019=NIS2019lvl4)

### mapping level 0 to 3

tmp_1995to2019_lvl0to3 <- tmp_lvl4 |>
  select(id,matches('^NIS[0-9]{4}lvl[0-3]')) |>
  pivot_longer(
    matches('^NIS[0-9]{4}lvl[0-3]'),
    names_to=c('version','maplevel'),
    names_sep='lvl',
    values_to='mapid',
    values_drop_na=TRUE,
  ) |>
  arrange(version,maplevel,mapid,id) |>
  summarize(ids=str_c(id,collapse=';'),.by=c(version,maplevel,mapid)) |>
  pivot_wider(names_from='version',values_from='mapid') |>
  filter(!is.na(NIS1995)) |>
  select(-maplevel,-ids)

tmp_lvl0to3 <- refnis1995 |>
  filter(level %in% as.character(0:3)) |>
  pivot_longer(
    matches('^NIS1995lvl'),
    names_to=c(NA,'maplevel'),
    names_sep='lvl',
    values_to ='NIS1995'
  ) |>
  left_join(tmp_1995to2019_lvl0to3,join_by(NIS1995)) |>
  pivot_wider(
    names_from=maplevel,
    names_sep='lvl',
    values_from=c('NIS1995','NIS2019')
  ) |>
  left_join(tmp_1995to2019_lvl0to3,join_by(id==NIS1995))

### level 5

tmp_1995lvl4_2019 <- tmp_lvl4 |>
  select(NIS1995lvl4,starts_with('NIS2019lvl'))

tmp_lvl5 <- refnis1995 |>
  filter(level=='5') |>
  left_join(tmp_1995lvl4_2019,join_by(NIS1995lvl4)) |>
  mutate(
    NIS2019lvl5=NIS1995lvl5,
    NIS2019    =NIS1995lvl5
  )

### bind together

tmp_NIS1995_with2019 <-
  bind_rows(tmp_lvl0to3,tmp_lvl4,tmp_lvl5) |>
  arrange(level,NIS1995lvl0,NIS1995lvl1,NIS1995lvl2,NIS1995lvl3,NIS1995lvl4,NIS1995lvl5)

### add 2025

tmp_2019to2025 <- refnis2019_withmaps |>
  select(NIS2019=id,matches('^NIS2025'))

refnis1995_withmaps <- tmp_NIS1995_with2019 |>
  left_join(tmp_2019to2025,join_by(NIS2019))

rm(list=ls(pattern="^tmp"))







# STEP 5: make codelist from 1983 -----------------------------------------
# -------------------------------------------------------------------------

refnis1983_withmaps <- refnis1995_withmaps |>
  filter(level!=1) |>
  mutate(
    id = replace_values(id,from=c('20001','20002'),to='20000'),
    label_nl = if_else(id=='20000','Provincie Brabant',label_nl),
    label_fr = if_else(id=='20000','Province du Brabant',label_fr),
    label_en = if_else(id=='20000','Province du Brabant',label_en),
    parent_id = case_when(
      level==2~'01000',
      parent_id %in% c('20001','20002','04000')~'20000',
      TRUE~parent_id),
    ) |>
  mutate(
    NIS1983lvl0=NIS1995lvl0,
    NIS1983lvl1=NA,
    NIS1983lvl2=case_when(
      NIS1995lvl1 %in% c('04000')~'20000',
      NIS1995lvl2 %in% c('20001','20002')~'20000',
      TRUE~NIS1995lvl2
      ),
    NIS1983lvl3=NIS1995lvl3,
    NIS1983lvl4=NIS1995lvl4,
    NIS1983lvl5=NIS1995lvl5,
    .before='NIS1995lvl0',
    ) |>
  mutate(
    across(
      NIS1995lvl1|NIS1995lvl2|NIS2019lvl1|NIS2019lvl2|NIS2025lvl1|NIS2025lvl2|NIS2019|NIS2025,
      ~if_else(level==2,NA,.x)
      ),
    ) |>
  mutate(
    NIS1995=ifelse(level==2,NA,id),
    .after=NIS1995lvl5
    ) |>
  distinct()











# Output ------------------------------------------------------------------
# -------------------------------------------------------------------------


years <- c(1983,1995,2019,2025) |>
  enframe(name=NULL,value="start") |>
  mutate(end=(lead(start)-1) |> as.character() |> replace_na('...') )


### wegschrijven per versie per level

# year=2019;level=4;data=test$data[[5]]
write_codelist_per_level <- function(year,level,data) {
  filename = str_c('cl_refnis',year,'lvl',level)
  data |>
    rename(value=id) |>
    rename_with(~str_replace(.x,'^NIS([0-9]{4})lvl([0-9])$','cl_refnis\\1lvl\\2')) |>
    rename_with(~str_replace(.x,'^NIS([0-9]{4})$','cl_refnis\\1')) |>
    select(-parent_id) |>
    select(-!!filename) |>
    select(where(~!all(is.na(.x)))) |>
    writexl::write_xlsx(str_c(outputfolder,'/',filename,'.xlsx'))
  }

for (year in years$start) {
  year |>
    sprintf(fmt='refnis%s_withmaps') |>
    get() |>
    nest(.by=level) |>
    mutate(data=map2(level,data,~write_codelist_per_level(year,.x,.y)))
  }



### wegschrijven per versie

for (year in years$start) {
  filename = str_c('cl_refnis',year)
  year |>
    sprintf(fmt='refnis%s_withmaps') |>
    get() |>
    select(value=id,label_nl,label_fr,label_en,matches('^NIS[0-9]{4}$')) |>
    rename_with(~str_replace(.x,'^NIS([0-9]{4})$','refnis\\1')) |>
    writexl::write_xlsx(str_c(outputfolder,'/',filename,'.xlsx'))
  }






