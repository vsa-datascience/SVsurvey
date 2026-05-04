SVsurvey_clean_data <- function(the_surv_id,data_sample,data_survey) {

   # ----------------------------------------------------------------------
   # LOAD METADATA --------------------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Load metadata\n')

   meta_questions        <- SVsurvey_read_questions(the_surv_id)
   meta_answers <- SVsurvey_read_answers(the_surv_id)
   meta_variables_sample <- SVsurvey_read_sample_variables(the_surv_id)
   meta_variables_survey <- SVsurvey_read_survey_variables(the_surv_id)


   # ----------------------------------------------------------------------
   # STEP 1A: CHECK SAMPLE DATA -------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Check structure sample data\n')

   dsd_sample <-
      SVsurvey_load_dsd(meta_variables_sample$concept_id) |>
      dplyr::rename(new_concept_id=concept_id) |>
      dplyr::left_join(
         dplyr::rename(meta_variables_sample,new_concept_id=concept_id,concept_id=variable),
         dplyr::join_by(new_concept_id)
         )

   data_sample_clean <- data_sample |>
      validate_data(dsd_sample,silent=TRUE) |>
      dplyr::rename(!!!dplyr::pull(meta_variables_sample,variable,concept_id))


   # ----------------------------------------------------------------------
   # STEP 1B: CHECK SURVEY DATA -------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Check structure survey data\n')

   tmp_cl_update <- function(cl) {
      cl |>
      dplyr::rename(new_value=value) |>
      dplyr::left_join(
         dplyr::rename(meta_answers,new_value=value,value=vallabel),
         dplyr::join_by(new_value)
         ) |>
      dplyr::mutate(value=dplyr::coalesce(value,new_value))
      }

   tmp_survey_concept_ids <- meta_variables_survey$concept_id
   if ( 'spt_othx' %in% names(data_survey) ) {
      tmp_survey_concept_ids <- c(tmp_survey_concept_ids,'spt_othx')
      }

   dsd_survey <-
      SVsurvey_load_dsd(meta_variables_survey$concept_id) |>
      dplyr::rename(new_concept_id=concept_id) |>
      dplyr::left_join(
         dplyr::rename(meta_variables_survey,new_concept_id=concept_id,concept_id=variable),
         dplyr::join_by(new_concept_id)
         ) |>
      dplyr::mutate(
         constraint_codelist_ds=purrr::map(constraint_codelist_ds,tmp_cl_update)
         )

   ### add the self created variables to the dsd
   tmp_add_vars <- c('spt_othx') |>
      intersect(names(data_survey))
   if ( length(tmp_add_vars)>0 ) {
      tmp_add_dsd <- SVsurvey_load_dsd(tmp_add_vars) |>
         dplyr::mutate(new_concept_id=concept_id)
      dsd_survey <- dplyr::bind_rows(dsd_survey,tmp_add_dsd)
      }

   tmp_rv <- dsd_survey |>
      dplyr::filter(constraint_type=="codelist") |>
      dplyr::pull(constraint_codelist_ds,concept_id)

   data_survey_clean <- data_survey |>
      validate_data(dsd_survey,silent=TRUE) |>
      dplyr::mutate(
         dplyr::across(
            tidyselect::all_of(names(tmp_rv)),
            \(x) factor(x,labels=tmp_rv[[dplyr::cur_column()]][["new_value"]],levels=tmp_rv[[dplyr::cur_column()]][["value"]])
            )
         ) |>
      dplyr::rename(!!!dplyr::pull(meta_variables_survey,variable,concept_id))

   rm(list=ls(pattern="^tmp"))




   # ----------------------------------------------------------------------
   # STEP 1C: OTHER CHECKS INPUT DATA  ------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Check survey data for inconsistencies\n')

   # initialise error vector
   errors <- NULL

   # check resp_stat disposition is filled in for every respondent
   tmp <- dplyr::filter(data_survey_clean,is.na(resp_fwdisp))
   if ( nrow(tmp)>0 ) {
      errors <- c(errors,'Respons disposition is missing for at least one respondent.')
      }

   # check qr/url complete for online respondents
   tmp <- dplyr::filter(data_survey_clean,resp_mode=="online" & is.na(resp_qrurl))
   if ( nrow(tmp)>0 ) {
      errors <- c(errors,'Information QR/URL is missing for at least one online respondent.')
      }

   # check start time complete for online respondents
   tmp <- dplyr::filter(data_survey_clean,resp_mode=="online" & is.na(resp_date))
   if ( nrow(tmp)>0 ) {
      errors <- c(errors,'Respons date is missing for at least one online respondent.')
      }

   # check start time missing for paper respondents
   tmp <- dplyr::filter(data_survey_clean,resp_mode!="online" & !is.na(resp_date))
   if ( nrow(tmp)>0 ) {
      errors <- c(errors,'Respons date is filled in for at least one non-online respondent.')
      }

   # check device complete for online respondents
   if ( "resp_dev" %in% names(data_survey_clean) ) {
      tmp <- dplyr::filter(data_survey_clean,resp_mode=="online" & is.na(resp_dev))
      if ( nrow(tmp)>0 ) {
         errors <- c(errors,'Respons device is missing for some online respondents.')
         }
      }

   # throw errors
   if ( length(errors)>0 ) {
      errors <- paste(errors,collapse="\n")
      stop("\n",errors)
      }

   # clean up
   rm(errors,tmp)





   # ----------------------------------------------------------------------
   # STEP 2: CLEAN AND MERGE DIFFERENT DATA SOURCES -----------------------
   # ----------------------------------------------------------------------

   cat('-> Merge different data files\n')

   meta_editions <- SVsurvey_read_edition_metadata(the_surv_id)
   the_fw_period <- meta_editions$fw_period
   meta_contact  <- SVsurvey_read_mailings_metadata(the_fw_period)

   # merge all data
   data_merged <- data_sample_clean |>
      dplyr::mutate(surv_id=the_surv_id,.before=1) |>
      dplyr::left_join(
         data_survey_clean,
         dplyr::join_by(pseudo_id)
         ) |>
      dplyr::left_join(
         dplyr::select(meta_editions,surv_id,fw_period,surv_year,surv_semester),
         dplyr::join_by(surv_id),
         suffix=c(".x","")
         ) |>
      dplyr::left_join(
         meta_contact,
         dplyr::join_by(fw_period),
         suffix=c(".x","")
         )






   # ----------------------------------------------------------------------
   # STEP 3: CORRECTION NCONSISTENCIES ------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Correct inconsistencies\n')

   data_inconsist <- data_merged

   # corrigeer geboortejaar, leeftijd moet tussen 18 en 120 liggen
   if ( "bth_year" %in% names(data_inconsist) ) {
      data_inconsist <- data_inconsist |>
         dplyr::mutate(
            tmp_lftd=surv_year-bth_year,
            bth_year = dplyr::case_when(
               tmp_lftd<18 | tmp_lftd>120 ~ NA,
               TRUE ~ bth_year
               ),
            tmp_lftd=NULL,
            )
      }

   # nhh_nchld naar nul als vragen over personen in huishouden zijn beantwoord
   # en nhh_nchld leeg is.
   if ( any(stringr::str_detect(names(data_inconsist),'^hh_[a-z]+$'))  &  'nhh_nchld' %in% names(data_inconsist)  ) {
      data_inconsist <- data_inconsist |>
         dplyr::mutate(
            tmp_hh=dplyr::if_any(matches('^hh_[a-z]+$'),\(x) x=='yes'),
            nhh_nchld=dplyr::case_when(
               tmp_hh & is.na(nhh_nchld) ~ 0,
               TRUE ~ nhh_nchld
               ),
            tmp_hh = NULL,
            )
      }

   # nhh_nadlt naar nul als vragen over personen in huishouden zijn beantwoord
   # en nhh_nadlt leeg is.
   if ( any(stringr::str_detect(names(data_inconsist),'^hh_[a-z]+$'))  &  'nhh_nadlt' %in% names(data_inconsist)  ) {
      data_inconsist <- data_inconsist |>
         dplyr::mutate(
            tmp_hh=dplyr::if_any(matches('^hh_[a-z]+$'),\(x) x=='yes'),
            nhh_nadlt=dplyr::case_when(
               tmp_hh & is.na(nhh_nadlt) ~ 0,
               TRUE ~ nhh_nadlt
               ),
            tmp_hh = NULL,
            )
      }

   # nhh_nsnr naar nul als vragen over personen in huishouden zijn beantwoord
   # en nhh_nsnr leeg is.
   if ( any(stringr::str_detect(names(data_inconsist),'^hh_[a-z]+$'))  &  'nhh_nsnr' %in% names(data_inconsist)  ) {
      data_inconsist <- data_inconsist |>
         dplyr::mutate(
            tmp_hh=dplyr::if_any(matches('^hh_[a-z]+$'),\(x) x=='yes'),
            nhh_nsnr=dplyr::case_when(
               tmp_hh & is.na(nhh_nsnr) ~ 0,
               TRUE ~ nhh_nsnr
               ),
            tmp_hh = NULL,
            )
      }

   # hh_child='yes' als nhh_nchld>0
   if (  'hh_child' %in% names(data_inconsist)  &  'nhh_nchld' %in% names(data_inconsist)  ) {
      data_inconsist <- data_inconsist |>
         dplyr::mutate(
            hh_child = dplyr::case_when(
               nhh_nchld>0 ~ 'yes',
               TRUE ~ hh_child
               ),
            )
      }

   # hh_alone='no' als er personen in huishouden worden gesignaleerd
   if ( 'hh_alone' %in% names(data_inconsist)  &  all(c('nhh_nchld','nhh_nadlt','nhh_nsnr') %in% names(data_inconsist)) ) {
      data_inconsist <- data_inconsist |>
         dplyr::mutate(
            hh_alone = dplyr::case_when(
               nhh_nchld>0 | nhh_nadlt>0 | nhh_nsnr>0 ~ 'no',
               TRUE ~ hh_alone
               ),
            )
      }




   # ----------------------------------------------------------------------
   # STEP 4: CLEAN CHECK-ALL-THAT-APPLY QUESTIONS -------------------------
   # ----------------------------------------------------------------------

   cat('-> Clean check-all-that-apply variables\n')

   data_cata  <- data_inconsist |>
      SVsurvey_clean_cata(
         .var=c('hh_partn','hh_child','hh_par','hh_sib','hh_fam','hh_frnd','hh_oth'),
         .dk='hh_dk',
         .none='hh_alone'
         ) |>
      SVsurvey_clean_cata(
         .var=c('mem_sport','mem_hobby' ,'mem_arts','mem_soclt','mem_union',
                'mem_nbhd' ,'mem_schl','mem_youth' ,'mem_snr' ,'mem_welf',
                'mem_env','mem_polp' ,'mem_oth'),
         .dk='mem_dk',
         .none='mem_none'
         ) |>
      SVsurvey_clean_cata(
         .var=c('spt_walk' ,'spt_cycl','spt_run','spt_fit','spt_swim',
                'spt_ftbl','spt_vball','spt_bball','spt_tenn','spt_padl',
                'spt_yoga','spt_mart','spt_danc','spt_gymn','spt_oth'),
         .dk='spt_dk'
         ) |>
      SVsurvey_clean_cata(
         .var=c('dev_comp','dev_tabl','dev_spho','dev_gsm',
                'dev_stv','dev_tv','dev_rad'),
         .dk='dev_dk',
         .none='dev_none'
         ) |>
      SVsurvey_clean_cata(
         .var=c('sub_dtv','sub_svid','sub_smuz','sub_news','sub_mag'),
         .dk='sub_dk',
         .none='sub_none'
         ) |>
      SVsurvey_clean_cata(
         .var=c('pa_pet','pa_socm','pa_demo','pa_mtng','pa_info','pa_cont',
                         'pa_press','pa_civmv','pa_advis','pa_polp','pa_oth'),
         .dk='pa_dk',
         .none='pa_none'
         ) |>
      SVsurvey_clean_cata(
         .var=c('netc_fixed','netc_mob'),
         .dk='netc_dk'
         ) |>
      SVsurvey_clean_cata(
         .var=c('net_comm','net_telew','net_info','net_govt','net_bank',
                'net_shop','net_sell','net_game','net_media','net_learn',
                'net_news','net_socm','net_publ','net_route'),
         .dk='net_act_dk'
         )









   # ----------------------------------------------------------------------
   # STEP 5: CLEAN FILTER QUESTIONS ---------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Clean filtered questions\n')

   data_filter <- data_cata |>
      SVsurvey_clean_filtered('sr_illim',sr_chron=='yes') |>
      SVsurvey_clean_filtered('memboard',
         dplyr::if_any(mem_sport|mem_hobby|mem_arts|mem_soclt|mem_union|mem_nbhd|mem_schl|
               mem_youth|mem_snr|mem_welf|mem_env|mem_polp|mem_oth,\(x) x=='yes')
         ) |>
      SVsurvey_clean_filtered(
         c('spt_walk','spt_cycl','spt_run','spt_fit','spt_swim','spt_ftbl',
           'spt_vball','spt_bball','spt_tenn','spt_padl','spt_yoga','spt_mart',
           'spt_danc','spt_gymn','spt_oth'),
         sptfreq=='yearly'|
         sptfreq=='monthly'|
         sptfreq=='weekly'|
         sptfreq=='daily'
         ) |>
      SVsurvey_clean_filtered('spt_othx',spt_oth!='na',.nvt='') |>
      SVsurvey_clean_filtered('icarerel',
         icarefreq=='yearly'|
         icarefreq=='monthly'|
         icarefreq=='weekly'|
         icarefreq=='daily'
         ) |>
      SVsurvey_clean_filtered('wb_work',
         sr_empl=='employed'
         ) |>
      SVsurvey_clean_filtered('wb_schhm',hh_alone!='yes') |>
      SVsurvey_clean_filtered(c('netc_fixed','netc_mob'),nethome=='yes') |>
      SVsurvey_clean_filtered(
          c('net_comm','net_telew','net_info','net_govt','net_bank','net_shop',
            'net_sell','net_game','net_media','net_learn','net_news','net_socm',
             'net_publ','net_route'),
         netfreq=='ltmonth'|
         netfreq=='monthly'|
         netfreq=='weekly'|
         netfreq=='daily'
         )



   # ----------------------------------------------------------------------
   # STEP 6: ANALYSIS OF RESPONS ------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Make respons variables\n')

   # voeg een row-ID toe aan data voor dubbele responses
   tmp_data <-  dplyr::mutate(data_filter,rowID=dplyr::row_number())

   # resp_stat percentage van vragen
   tmp_variables <- meta_variables_survey |>
      dplyr::left_join(meta_questions,dplyr::join_by(concept_id)) |>
      dplyr::filter_out(is.na(question_module)) |>
      dplyr::filter_out(stringr::str_detect(concept_id,'_text$')) |>
      dplyr::mutate(
         concept_id,
         resp_pct  = round(dplyr::row_number()/dplyr::n(),3),
         resp_drop = dplyr::lead(concept_id),
         .keep="none"
         )

   # bereken percentage vragen ingevuld en vraag van dropout
   tmp_dropout <- tmp_data |>
      dplyr::select(rowID,all_of(tmp_variables$concept_id)) |>
      dplyr::mutate(
         dplyr::across(-rowID,as.character),
         dplyr::across(-rowID,\(x) tidyr::replace_na(x,'')),
         dplyr::across(-rowID,\(x) dplyr::case_when(
            x=='' ~ 'no respons',
            x %in% c('dkn','invalid') ~ 'nonsubstantial',
            TRUE ~ 'respons'
            ))
         ) |>
      tidyr::pivot_longer(-rowID,names_to='concept_id',values_to='behavior') |>
      dplyr::mutate(
         tmp_respdemo = any(concept_id=='sr_sex'   & behavior=='respons') &
                     any(concept_id=='sr_byear' & behavior=='respons') &
                     any(concept_id=='sr_bmnth' & behavior=='respons') ,
         tmp_presp = sum(behavior=='respons')/dplyr::n(),
         .by=rowID
         ) |>
      dplyr::filter(behavior %in% c('respons','nonsubstantial')) |>
      dplyr::left_join(tmp_variables,by='concept_id') |>
      dplyr::slice_max(resp_pct,by=rowID) |>
      dplyr::select(rowID,tmp_respdemo,tmp_presp,resp_pct,resp_drop)

   # maak respons variabelen aan
   data_respons <- tmp_data |>
      dplyr::left_join(tmp_dropout,dplyr::join_by(rowID)) |>
      dplyr::mutate(
         resp_pct=tidyr::replace_na(resp_pct,0),
         resp_disp=dplyr::case_when(
            resp_fwdisp %in% c('late','deceased','incapble','language','wrongrsp','unreach') ~ resp_fwdisp,
            tmp_respdemo & tmp_presp>=.8 ~ 'full',
            tmp_respdemo & tmp_presp>=.3 ~ 'partial',
            !tmp_respdemo & resp_pct >0  ~ 'wrongrsp',
            resp_pct >0 ~ 'breakoff',
            resp_pct==0 ~ 'refusal'
            ),
         resp_stat = dplyr::recode_values(resp_disp,
            c('full','partial') ~ 'resp',
            default='nonresp'
            ),
         resp_time=dplyr::case_when(
             resp_mode=='online' & invitation<=resp_date & resp_date<=reminder_1 ~ 'inv1'
            ,resp_mode=='online' & reminder_1< resp_date & resp_date<=reminder_2 ~ 'rem1'
            ,resp_mode=='online' & reminder_2< resp_date                         ~ 'rem2'
            ,resp_mode=='paper'                                                  ~ 'rem2'
            ),
         ) |>
      dplyr::select(
         -tmp_respdemo,-tmp_presp,-rowID,
         -invitation,-reminder_1,-reminder_2,
         -resp_fwdisp
         )

   # clean up
   rm(list=ls(pattern="^tmp"))






   # ----------------------------------------------------------------------
   # STEP 7: DELETE DOUBLES -----------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Delete doubles\n')

   # neem steekproefgrootte
   tmp_stkprfn <- dplyr::pull(meta_editions,sample_n)

   # Enkel dubbels berekenen als er dubbels zijn
   if ( nrow(data_respons)==tmp_stkprfn ) {
      data_nodoubles <- data_respons
   } else {
      data_nodoubles <- data_respons |>
         dplyr::mutate(priority = dplyr::recode_values(resp_stat,'resp'~1,'nonresp'~0)) |>
         dplyr::slice_max(resp_stat,by=pseudo_id) |>
         dplyr::select(-priority) |>
         dplyr::slice_max(resp_pct,by=pseudo_id) |>
         dplyr::slice_min(resp_date,by=pseudo_id)
   }

   # clean up
   rm(list=ls(pattern="^tmp"))







   # ----------------------------------------------------------------------
   # STEP 8: DERIVED VARIABLES --------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Calculate derived variables\n')

   # kopieer de dataset
   data_derivedvar <- data_nodoubles

   # random seed for the resp_id
   set.seed(readr::parse_number(the_surv_id)+1213)
   data_derivedvar <- dplyr::mutate(data_derivedvar,
      resp_id    =sprintf('%s-%04.f',the_surv_id,sample(1:dplyr::n(),dplyr::n())),
      resp_id_suf=sprintf('%s-SUF-%04.f',the_surv_id,sample(1:dplyr::n(),dplyr::n())),
      resp_id_puf=sprintf('%s-PUF-%04.f',the_surv_id,sample(1:dplyr::n(),dplyr::n())),
      ) |>
   dplyr::select(-pseudo_id)

   # province
   tmp_refnislvl4 <- data_derivedvar |> names() |> stringr::str_subset('^refnis\\d{4}lvl4$')
   tmp_refnislvl2 <- tmp_refnislvl4 |> stringr::str_replace("lvl4$","lvl2")
   tmp_map <- paste0('cl_',tmp_refnislvl4) |>
      SVsurvey_load_codelist() |>
      dplyr::select({{tmp_refnislvl4}}:=value,{{tmp_refnislvl2}}:=paste0('cl_',tmp_refnislvl2))
   data_derivedvar <- data_derivedvar |>
      dplyr::left_join(tmp_map,dplyr::join_by({{tmp_refnislvl4}}))
   rm(list=ls(pattern='^tmp_'))

   # natltygrp
   tmp_map_natltygrp <- SVsurvey_load_codelist("cl_countryNISnatlty") |>
      dplyr::mutate(
      natlty=factor(value,levels=levels(data_derivedvar$natlty)),
      natltygrp=cl_countrycat1,
      .keep='none'
      )
   data_derivedvar <- data_derivedvar |>
      dplyr::left_join(tmp_map_natltygrp,dplyr::join_by(natlty))
   rm(list=ls(pattern='^tmp_'))

   # leeftijd
   if ( 'surv_year' %in% names(data_derivedvar) & 'bth_year' %in% names(data_derivedvar) ){
      data_derivedvar <- dplyr::mutate(data_derivedvar,
         age = surv_year - bth_year,
         agecat7 = age2agecat7from18(age),
         agecat4 = age2agecat4from18(age),
         )
      }

   # onderwijsniveau
   if ( 'sr_educ' %in% names(data_derivedvar) ){
      tmp_map_edlvl <- SVsurvey_load_codelist("cl_educlev4") |>
         dplyr::select(sr_edlv4=value,sr_edlv3=cl_educlev3)
      data_derivedvar <- data_derivedvar |>
         dplyr::mutate(,
            sr_edlv4 = dplyr::recode_values(sr_educ,
               'nodip'     ~ 'loweduc',
               'primary'   ~ 'loweduc',
               'lowrsec'   ~ 'loweduc',
               'uprsec'    ~ 'mededuc',
               'nonuniv'   ~  'hieduc',
               'univ'      ~ 'hieduc',
               'otherdipl' ~ 'othereduc',
               'invalid'   ~ NA,
               NA ~ NA,
               unmatched = 'error'
               )
            ) |>
         dplyr::left_join(tmp_map_edlvl,dplyr::join_by(sr_edlv4))
      rm(list=ls(pattern='^tmp_'))
      }

   # professionele situatie
   if ( 'sr_empl' %in% names(data_derivedvar) ){
      data_derivedvar <- dplyr::mutate(data_derivedvar,
         sr_emplb = dplyr::replace_values(sr_empl,
            'jobseek'~'otherempl',
            'student'~'otherempl',
            'homemkr'~'otherempl',
            'disabil'~'otherempl',
            )
         )
      }

   # huishoudpositie
   if ( 'hh_alone' %in% names(data_derivedvar) &
        'hh_partn' %in% names(data_derivedvar) &
        'hh_child' %in% names(data_derivedvar) &
        'hh_par' %in% names(data_derivedvar) ){
      data_derivedvar <- dplyr::mutate(data_derivedvar,
         tmp_other=dplyr::if_any(
            any_of(c('hh_partn','hh_child','hh_par','hh_sib','hh_fam','hh_frnd','hh_oth','hh_alone')),
            \(x) x=='yes'
            ),
         hhpos=dplyr::case_when(
            hh_alone=='yes'                   ~ 'alone',
            hh_partn=='yes' & hh_child=='no'  ~ 'partnr',
            hh_partn=='yes' & hh_child=='yes' ~ 'partkid',
            hh_partn=='no'  & hh_child=='yes' ~ 'sinparn',
            hh_partn=='no'  & hh_par  =='yes' ~ 'wparnt',
            tmp_other                         ~ 'otherhhpos'
            ),
         hhposb = dplyr::replace_values(hhpos,'wparnt' ~ 'otherhhpos'),
         tmp_other = NULL
         )
      }

   # totaal aantal internettoepassingen
   tmp_vars <- c('net_comm','net_telew','net_info','net_govt','net_bank','net_shop','net_sell','net_game','net_media','net_learn','net_news','net_socm','net_publ','net_route')
   tmp_vars_dich <- paste0(tmp_vars,'_dich')
   if ( all( tmp_vars %in% names(data_derivedvar)) ) {
      data_derivedvar <- data_derivedvar |>
         dplyr::mutate(
            dplyr::across(all_of(tmp_vars),
               \(x) dplyr::recode_values(x,'yes'~TRUE,c('no','na')~FALSE,c('dkn',NA)~NA,unmatched="error"),
               .names='{.col}_dich'
               ),
            netntot = rowSums(dplyr::across(all_of(tmp_vars_dich))),
            ) |>
         dplyr::select(-all_of(tmp_vars_dich))
      }
   rm(list=ls(pattern='^tmp_'))

   # totaal aantal politieke activeiten
   tmp_vars <- c('pa_pet','pa_socm','pa_demo','pa_mtng','pa_info','pa_cont','pa_press','pa_civmv','pa_advis','pa_polp','pa_oth')
   tmp_vars_dich <- paste0(tmp_vars,'_dich')
   if ( all( tmp_vars %in% names(data_derivedvar)) ) {
      data_derivedvar <- data_derivedvar |>
         dplyr::mutate(
            dplyr::across(all_of(tmp_vars),
               \(x) dplyr::recode_values(x,'yes'~TRUE,'no'~FALSE,c('dkn',NA)~NA,unmatched="error"),
               .names='{.col}_dich'
               ),
            patot = rowSums(dplyr::across(all_of(tmp_vars_dich))),
            ) |>
         dplyr::select(-all_of(tmp_vars_dich))
      }
   rm(list=ls(pattern='^tmp_'))

   # lidmaatschap aantal soorten verenigingen
   tmp_vars <- c('mem_sport','mem_hobby','mem_arts','mem_soclt','mem_union','mem_nbhd','mem_schl','mem_youth','mem_snr','mem_welf','mem_env','mem_polp','mem_oth')
   tmp_vars_dich <- paste0(tmp_vars,'_dich')
   if ( all( tmp_vars %in% names(data_derivedvar)) ) {
      data_derivedvar <- data_derivedvar |>
         dplyr::mutate(
            dplyr::across(all_of(tmp_vars),
               \(x) dplyr::recode_values(x,'yes'~TRUE,'no'~FALSE,c('dkn',NA)~NA,unmatched="error"),
               .names='{.col}_dich'
               ),
            memtot = rowSums(dplyr::across(all_of(tmp_vars_dich))),
            ) |>
         dplyr::select(-all_of(tmp_vars_dich))
      }
   rm(list=ls(pattern='^tmp_'))

   # cultuurparticipatie
   tmp_vars <- c('cult_conc','cult_cine','cult_opra','cult_danc','cult_thtr','cult_crcs','cult_mscl','cult_cbrt','cult_musm','cult_mnmt','cult_lib')
   if ( all( tmp_vars %in% names(data_derivedvar)) ) {
      tmp_rcd_act <- tibble::tribble(
         ~val     ,~weekly,~monthly,~participant,~respons,
         'daily'  ,TRUE   ,TRUE    ,TRUE        ,TRUE ,
         'weekly' ,TRUE   ,TRUE    ,TRUE        ,TRUE ,
         'monthly',FALSE  ,TRUE    ,TRUE        ,TRUE ,
         'yearly' ,FALSE  ,FALSE   ,TRUE        ,TRUE ,
         'never'  ,FALSE  ,FALSE   ,FALSE       ,TRUE ,
         "dkn"    ,NA     ,NA      ,NA          ,FALSE,
         'invalid',NA     ,NA      ,NA          ,NA   ,
         NA       ,NA     ,NA      ,NA          ,NA   ,
         )
      data_derivedvar <- data_derivedvar |>
         dplyr::mutate(
            dplyr::across(
               all_of(tmp_vars),
               \(x) dplyr::recode_values(x,from=tmp_rcd_act$val,to=tmp_rcd_act$weekly,unmatched="error"),
               .names='tmp_w_{.col}'
               ),
            dplyr::across(
               all_of(tmp_vars),
               \(x) dplyr::recode_values(x,from=tmp_rcd_act$val,to=tmp_rcd_act$monthly,unmatched="error"),
               .names='tmp_m_{.col}'
               ),
            dplyr::across(
               all_of(tmp_vars),
               \(x) dplyr::recode_values(x,from=tmp_rcd_act$val,to=tmp_rcd_act$participant,unmatched="error"),
               .names='tmp_p_{.col}'
               ),
            dplyr::across(
               all_of(tmp_vars),
               \(x) dplyr::recode_values(x,from=tmp_rcd_act$val,to=tmp_rcd_act$respons,unmatched="error"),
               .names='tmp_a_{.col}'
               ),
            cultpart=dplyr::case_when(
               rowSums(dplyr::across(starts_with('tmp_p_act_')))>=1               ~'yes',
               rowSums(dplyr::across(starts_with('tmp_a_act_')))==length(tmp_vars)~'no',
               rowSums(dplyr::across(starts_with('tmp_a_act_'))) <length(tmp_vars)~'dkn',
               ),
            cultcore=dplyr::case_when(
               rowSums(dplyr::across(starts_with('tmp_w_act_')))>=1               ~'yes',
               rowSums(dplyr::across(starts_with('tmp_m_act_')))>=2               ~'yes',
               rowSums(dplyr::across(starts_with('tmp_a_act_')))==length(tmp_vars)~'no',
               rowSums(dplyr::across(starts_with('tmp_a_act_'))) <length(tmp_vars)~'dkn',
               ),
            cultintp=dplyr::case_when(
               cultpart=='yes' & cultcore=='yes' ~'no' ,
               cultpart=='yes' & cultcore=='no'  ~'yes',
               cultpart=='yes' & cultcore=='dkn' ~'dkn',
               cultpart=='no'  & cultcore=='no'  ~'no' ,
               cultpart=='dkn' & cultcore=='dkn' ~'dkn',
               ),
            cultnonp=dplyr::case_when(
               cultpart=='yes' ~'no' ,
               cultpart=='no'  ~'yes',
               cultpart=='dkn' ~'dkn',
               ),
            ) |>
         dplyr::select(
            -starts_with('tmp_w_'),
            -starts_with('tmp_m_'),
            -starts_with('tmp_p_'),
            -starts_with('tmp_a_')
            )
      }
   rm(list=ls(pattern="^tmp"))


   # ----------------------------------------------------------------------
   # STEP 9: CALCULATION WEIGHTS ------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Calculate weights\n')
   ### gebaseerd op https://www.r-bloggers.com/2018/12/survey-raking-an-illustration/

   # selecteer covariaten
   tmp_covar <- c(
      'sex','agecat7','natltygrp',
      stringr::str_subset(names(data_derivedvar),'^refnis\\d{4}lvl2$')
      )

   # jaar van survey
   tmp_surv_year <- dplyr::pull(meta_editions,surv_year)

   # pad van populatiedata
   tmp_poppath <- r"{..\03_deriveddata\03_aggregated_population_data\stock_%i.xlsx}" |>
      sprintf(tmp_surv_year)

   # hervorm de enquete gegevens voor de raking
   tmp_survey <- data_derivedvar |>
      dplyr::filter(resp_stat=='resp') |>
      dplyr::mutate(resp_id,dplyr::across(all_of(tmp_covar),as.factor),.keep="none") |>
      as.data.frame() |>
      na.omit()

   # laad populatie data
   tmp_pop <- tmp_poppath |>
      readxl::excel_sheets() |>
      intersect(tmp_covar) |>
      rlang::set_names() |>
      purrr::map(readxl::read_xlsx,path=tmp_poppath) |>
      suppressWarnings() |>
      purrr::map(dplyr::pull,p,1) |>
      purrr::map(\(x) x/sum(x))

   # bereken gewichten
   tmp_weights <- tmp_pop |>
      anesrake::anesrake(
         dataframe = tmp_survey,
         caseid = tmp_survey$resp_id,
         cap=5,
         type='nolim'
         ) |>
      quiet()

   # # bewaar informatie over weging
   # outputfolder |>
   #    file.path(str_glue("{the_surv_id}-informatie_weging.rds")) |>
   #    write_rds(x=tmp_weights)

   # koppel gewichten aan data
   data_weights <- tmp_weights |>
      purrr::pluck('weightvec') |>
      tibble::enframe('resp_id','psweight') |>
      dplyr::left_join(x=data_derivedvar,dplyr::join_by(resp_id))

   # opkuisen
   rm(list=ls(pattern="^tmp"))


   # ----------------------------------------------------------------------
   # STEP 10: CHECK DATASET -----------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Check cleaned data\n')

   # Controleer aantal respondenten gekuiste data zoals in metadata
   tmp_stkprfn <- dplyr::pull(meta_editions,sample_n)
   tmp_nrow <- nrow(data_weights)
   if ( tmp_stkprfn!=tmp_nrow) {
      stop('Number of sample members in data (n=',tmp_nrow,') ',
           'does not match number of sample members in metadata',
           ' (n=',tmp_stkprfn,')')
      }
   rm(list=ls(pattern="^tmp"))

   # check structure of cleaned data
   dsd_clean <- SVsurvey_load_dsd(names(data_weights))
   cleaned_data <- data_weights |>
      validate_data(dsd_clean,silent=TRUE)


   # ----------------------------------------------------------------------
   # STEP 11: WRITE CLEANED DATA ------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Write cleaned data\n')

   # schrijf data weg
   cleaned_data |>
      dplyr::group_by(surv_id) |>
      arrow::write_dataset(
         path=r"{..\03_deriveddata\05_Cleaned_data}",
         format="parquet"
         )


   # ----------------------------------------------------------------------
   # FINAL ----------------------------------------------------------------
   # ----------------------------------------------------------------------

   # return data
   cat('\033[32mCleaning succesfully finished!\033[0m\n')
   invisible(dsd_clean)


   }