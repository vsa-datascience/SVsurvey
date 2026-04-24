SVsurvey_clean_data <- function(the_surv_id,data_sample,data_survey) {


   # # ----------------------------------------------------------------------
   # # TUSSENSTAP: Voeg tyupe toe aan sample data als het er nog niet in zit
   # # ----------------------------------------------------------------------
   #
   # if ( !"type" %in% names(sample) ) { ### correct for editions where urbanisation
   #    sample$type = NA_character_    ### level is not yet provide by Statbel
   #    }

   # # ----------------------------------------------------------------------
   # # TUSSENSTAP: Laad metadata --------------------------------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> load metadata\n')
   #
   # meta_questions        <- SVsurvey_read_questions(the_surv_id)
   # meta_answers          <- SVsurvey_read_answers(the_surv_id)


   # ----------------------------------------------------------------------
   # STEP 1A: CHECK SAMPLE DATA -------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> check structure sample data\n')

   tmp_variables_sample <- SVsurvey_read_sample_variables(the_surv_id)

   meta_dsd_sample <-
      SVsurvey_load_dsd(tmp_variables_sample$concept_id) |>
      dplyr::rename(new_concept_id=concept_id) |>
      dplyr::left_join(
         rename(tmp_variables_sample,new_concept_id=concept_id,concept_id=variable),
         dplyr::join_by(new_concept_id)
         )

   data_sample <- data_sample |>
      validate_data(meta_dsd_sample,silent=TRUE) |>
      rename(!!!pull(tmp_variables_sample,variable,concept_id))


   # ----------------------------------------------------------------------
   # STEP 1B: CHECK SURVEY DATA -------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> check structure survey data\n')

   tmp_answers <- SVsurvey_read_answers(the_surv_id)

   tmp_cl_update <- function(cl) {
      cl |>
      dplyr::rename(new_value=value) |>
      dplyr::left_join(
         dplyr::rename(tmp_answers,new_value=value,value=vallabel),
         dplyr::join_by(new_value)
         ) |>
      dplyr::mutate(value=dplyr::coalesce(value,new_value))
      }

   tmp_variables_survey <- SVsurvey_read_survey_variables(the_surv_id)

   meta_dsd_survey <-
      SVsurvey_load_dsd(tmp_variables_survey$concept_id) |>
      dplyr::rename(new_concept_id=concept_id) |>
      dplyr::left_join(
         rename(tmp_variables_survey,new_concept_id=concept_id,concept_id=variable),
         dplyr::join_by(new_concept_id)
         ) |>
      dplyr::mutate(
         constraint_codelist=purrr::map(constraint_codelist,tmp_cl_update)
         )

   tmp_rv <- meta_dsd_survey |>
      dplyr::filter(constraint_type=="codelist") |>
      dplyr::pull(constraint_codelist,concept_id)

   data_survey <- data_survey |>
      validate_data(meta_dsd_survey,silent=TRUE) |>
      dplyr::mutate(
         dplyr::across(
            tidyselect::all_of(names(tmp_rv)),
            \(x) factor(x,labels=tmp_rv[[dplyr::cur_column()]][["new_value"]],levels=tmp_rv[[dplyr::cur_column()]][["value"]])
            )
         ) |>
      dplyr::rename(!!!pull(tmp_variables_survey,variable,concept_id))

   rm(list=ls(pattern="^tmp"))




   # ----------------------------------------------------------------------
   # STEP 1C: OTHER CHECKS INPUT DATA  ------------------------------------
   # ----------------------------------------------------------------------

   cat('-> check survey data for inconsistencies\n')

   # initialise error vector
   errors <- NULL

   # check respons disposition is filled in for every respondent
   tmp <- dplyr::filter(data_survey,is.na(resp_fwdisp))
   if ( nrow(tmp)>0 ) {
      errors <- c(errors,'Respons disposition is missing for at least one respondent.')
      }

   # check qr/url complete for online respondents
   tmp <- dplyr::filter(data_survey,resp_mode=="online" & is.na(resp_qrurl))
   if ( nrow(tmp)>0 ) {
      errors <- c(errors,'Information QR/URL is missing for at least one online respondent.')
      }

   # check start time complete for online respondents
   tmp <- dplyr::filter(data_survey,resp_mode=="online" & is.na(resp_date))
   if ( nrow(tmp)>0 ) {
      errors <- c(errors,'Respons date is missing for at least one online respondent.')
      }

   # check start time missing for paper respondents
   tmp <- dplyr::filter(data_survey,resp_mode!="online" & !is.na(resp_date))
   if ( nrow(tmp)>0 ) {
      errors <- c(errors,'Respons date is filled in for at least one non-online respondent.')
      }

   # check device complete for online respondents
   if ( "resp_dev" %in% names(data_survey) ) {
      tmp <- dplyr::filter(data_survey,resp_mode=="online" & is.na(resp_dev))
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
   data_merged <- data_sample |>
      mutate(surv_id=the_surv_id,.before=1) |>
      left_join(data_survey,join_by(pseudo_id)) |>
      left_join(
         dplyr::select(meta_editions,surv_id,fw_period,surv_year,surv_semester),
         join_by(surv_id),
         suffix=c(".x","")
         ) |>
      left_join(
         meta_contact,
         join_by(fw_period),
         suffix=c(".x","")
         )






   # ----------------------------------------------------------------------
   # STEP 3: CORRECTION NCONSISTENCIES ------------------------------------
   # ----------------------------------------------------------------------

   cat('-> Correct inconsistencies\n')

   data_inconsist <- data_merged

   # corrigeer geboortejaar, leeftijd moet tussen 18 en 120 liggen
   if ( "bth_year" %in% names(data_inconsist) ) {
      data_inconsist <- mutate(data_inconsist,
         tmp_lftd=surv_year-bth_year-1,
         bth_year = case_when(
            tmp_lftd<18 | tmp_lftd>120 ~ NA,
            TRUE ~ bth_year
            ),
         tmp_lftd=NULL,
         )
      }

   # nhh_nchld naar nul als vragen over personen in huishouden zijn beantwoord
   # en nhh_nchld leeg is.
   if ( any(str_detect(names(data_inconsist),'^hh_[a-z]+$'))  &  'nhh_nchld' %in% names(data_inconsist)  ) {
      data_inconsist <- mutate(data_inconsist,
         tmp_hh=if_any(matches('^hh_[a-z]+$'),~.x=='yes'),
         nhh_nchld=case_when(
            tmp_hh & is.na(nhh_nchld) ~ 0,
            TRUE ~ nhh_nchld
            ),
         tmp_hh = NULL,
         )
      }

   # nhh_nadlt naar nul als vragen over personen in huishouden zijn beantwoord
   # en nhh_nadlt leeg is.
   if ( any(str_detect(names(data_inconsist),'^hh_[a-z]+$'))  &  'nhh_nadlt' %in% names(data_inconsist)  ) {
      data_inconsist <- mutate(data_inconsist,
         tmp_hh=if_any(matches('^hh_[a-z]+$'),~.x=='yes'),
         nhh_nadlt=case_when(
            tmp_hh & is.na(nhh_nadlt) ~ 0,
            TRUE ~ nhh_nadlt
            ),
         tmp_hh = NULL,
         )
      }

   # nhh_nsnr naar nul als vragen over personen in huishouden zijn beantwoord
   # en nhh_nsnr leeg is.
   if ( any(str_detect(names(data_inconsist),'^hh_[a-z]+$'))  &  'nhh_nsnr' %in% names(data_inconsist)  ) {
      data_inconsist <- mutate(data_inconsist,
         tmp_hh=if_any(matches('^hh_[a-z]+$'),~.x=='yes'),
         nhh_nsnr=case_when(
            tmp_hh & is.na(nhh_nsnr) ~ 0,
            TRUE ~ nhh_nsnr
            ),
         tmp_hh = NULL,
         )
      }

   # hh_child='yes' als nhh_nchld>0
   if (  'hh_child' %in% names(data_inconsist)  &  'nhh_nchld' %in% names(data_inconsist)  ) {
      data_inconsist <- mutate(data_inconsist,
         hh_child = case_when(
            nhh_nchld>0 ~ 'yes',
            TRUE ~ hh_child
            ),
         )
      }

   # hh_alone='no' als er personen in huishouden worden gesignaleerd
   if ( 'hh_alone' %in% names(data_inconsist)  &  all(c('nhh_nchld','nhh_nadlt','nhh_nsnr') %in% names(data_inconsist)) ) {
      data_inconsist <- mutate(data_inconsist,
         hh_alone = case_when(
            nhh_nchld>0 | nhh_nadlt>0 | nhh_nsnr>0 ~ 'no',
            TRUE ~ hh_alone
            ),
         )
      }






   # ----------------------------------------------------------------------
   # STAP 4: CLEAN CHECK-ALL-THAT-APPLY QUESTIONS -------------------------
   # ----------------------------------------------------------------------

   cat('-> Kuis check-all-that-apply variabelen\n')



   # kuis de check-all-that-apply vragen
   data_cata  <- data_inconsist |>
      SVsurvey_clean_cata(
         .var=c('hh_par','hh_kin','hh_oud','hh_brzu','hh_af','hh_vri','hh_and'),
         .dk='hh_wn',
         .none='hh_all'
         ) |>
      SVsurvey_clean_cata(
         .var=c('lid_sprt','lid_hbb' ,'lid_knst','lid_sclt','lid_vkb',
                'lid_brt' ,'lid_schl','lid_jgd' ,'lid_oud' ,'lid_soc',
                'lid_mil','lid_polp' ,'lid_and'),
         .dk='lid_wn',
         .none='lid_geen'
         ) |>
      SVsurvey_clean_cata(
         .var=c('sprt_wa' ,'sprt_fts','sprt_lop','sprt_ftn','sprt_zwm',
                'sprt_vtb','sprt_vlb','sprt_bkb','sprt_tns','sprt_pdl',
                'sprt_yga','sprt_gvs','sprt_dns','sprt_trn','sprt_and'),
         .dk='sprt_wn'
         ) |>
      SVsurvey_clean_cata(
         .var=c('mb_comp','mb_tabl','mb_spho','mb_gsm',
                'mb_stv','mb_tv','mb_rad'),
         .dk='mb_wn',
         .none='mb_geen'
         ) |>
      SVsurvey_clean_cata(
         .var=c('tbd_dtv','tbd_smov','tbd_smuz','tbd_abkr','tbd_abma'),
         .dk='tbd_wn',
         .none='tbd_geen'
         ) |>
      SVsurvey_clean_cata(
         .var=c('pa_pet','pa_socm','pa_bet','pa_verg','pa_info','pa_cont',
                         'pa_pers','pa_bew','pa_overl','pa_part','pa_and'),
         .dk='pa_wn',
         .none='pa_geen'
         ) |>
      SVsurvey_clean_cata(
         .var=c('intv_vst','intv_mob'),
         .dk='intv_wn'
         ) |>
      SVsurvey_clean_cata(
         .var=c('int_comm','int_tewe','int_info','int_cove','int_bank',
                'int_aank','int_verk','int_game','int_muzi','int_lere',
                'int_nws','int_socm','int_eigm','int_rout'),
         .dk='int_wn'
         )

   # opkuisen
   rm(list=ls(pattern="^tmp"))








   # # ----------------------------------------------------------------------
   # # STAP 5: CLEAN FILTER QUESTIONS ---------------------------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> Kuis gefilterde variabelen\n')
   #
   # # value for not applicable
   # tmp_nvt <- missing_values['991']
   #
   # # function for cleaning filtered questions
   # tmp_cleanfiltered <- function(.data,.filtered,.filter,.nvt=tmp_nvt) {
   #    if ( any(.filtered %in% names(.data)) ) {
   #       .data <- ( .data
   #          %>% mutate(across(any_of(.filtered),~if_else({{.filter}},.x,.nvt)))
   #          )
   #       }
   #    return(.data)
   #    }
   #
   # # clean filtered questions
   # data_filter <- data_cata |>
   #    tmp_cleanfiltered('ziekblm',ziekhand=='Ja') |>
   #    tmp_cleanfiltered('bstrlid',
   #       if_any(lid_sprt|lid_hbb|lid_knst|lid_sclt|lid_vkb|lid_brt|lid_schl|
   #             lid_jgd|lid_oud|lid_soc|lid_mil|lid_polp|lid_and,~.x=='Ja')
   #       ) |>
   #    tmp_cleanfiltered(
   #       c('sprt_wa','sprt_fts','sprt_lop','sprt_ftn','sprt_zwm','sprt_vtb',
   #         'sprt_vlb','sprt_bkb','sprt_tns','sprt_pdl','sprt_yga','sprt_gvs',
   #         'sprt_dns','sprt_trn','sprt_and'),
   #       sprtfreq=='1 of meerdere keren per jaar'|
   #       sprtfreq=='1 of meerdere keren per maand'|
   #       sprtfreq=='1 of meerdere keren per week'|
   #       sprtfreq=='Dagelijks of bijna dagelijks'
   #       ) |>
   #    tmp_cleanfiltered('infzrel',
   #       infzorg=='1 of meerdere keren per jaar'|
   #       infzorg=='1 of meerdere keren per maand'|
   #       infzorg=='1 of meerdere keren per week'|
   #       infzorg=='Dagelijks of bijna dagelijks'
   #       ) |>
   #    tmp_cleanfiltered('sprt_andx',sprt_and!='Niet van toepassing',.nvt='') |>
   #    tmp_cleanfiltered('tvr_werk',
   #       profsit=='Betaald werk als werknemer, zelfstandige of ambtenaar (al dan niet tijdelijk werkloos)'
   #       ) |>
   #    tmp_cleanfiltered('tvr_sch',hh_all!='Ja') |>
   #    tmp_cleanfiltered(c('intv_vst','intv_mob'),thsintnt=='Ja') |>
   #    tmp_cleanfiltered(
   #        c('int_comm','int_tewe','int_info','int_cove','int_bank','int_aank',
   #          'int_verk','int_game','int_muzi','int_lere','int_nws','int_socm',
   #           'int_eigm','int_rout'),
   #       freqint=='Minder dan 1 keer per maand'|
   #       freqint=='1 of meerdere keren per maand'|
   #       freqint=='1 of meerdere keren per week'|
   #       freqint=='Dagelijks of bijna dagelijks'
   #       )
   #
   # # opkuisen
   # rm(list=ls(pattern="^tmp"))
   #
   #
   #
   #
   #
   #
   #
   # # ----------------------------------------------------------------------
   # # STAP 6: ANALYSE VAN RESPONS ------------------------------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> Maak responsvariabelen aan\n')
   #
   # # voeg een row-ID toe aan data voor dubbele responses
   # tmp_data <-  mutate(data_filter,rowID=row_number())
   #
   # # respons percentage van vragen
   # tmp_variables <- meta_variables |>
   #    filter(
   #       str_detect(origvarname,'^v[0-9]+_?[0-9]*$'),
   #       !str_detect(varname,'_text$'),
   #       !str_detect(varname,'_wn$'),
   #       ) |>
   #    mutate(
   #       tmp1 = str_replace(origvarname,'^v(\\d+)_?(\\d*)$','\\1') |>
   #              as.numeric() |> replace_na(0),
   #       tmp2 = str_replace(origvarname,'^v(\\d+)_?(\\d*)$','\\2') |>
   #              as.numeric() |> replace_na(0),
   #       order=tmp1+.001*tmp2
   #       ) |>
   #    select(-matches('^tmp\\d$')) |>
   #    arrange(order) |>
   #    mutate(
   #       part     = case_match(vragenlijstmodule,
   #          'Socio-demografische variabelen'~'socdem',
   #          .default='other'
   #          ),
   #       respqup  = round(row_number()/n(),3),
   #       respqudo = lead(varname),
   #       ) |>
   #    select(varname,part,respqup,respqudo)
   #
   # # bereken percentage vragen ingevuld en vraag van dropout
   # tmp_dropout <- tmp_data |>
   #    select(rowID,all_of(tmp_variables$varname)) |>
   #    mutate(
   #       across(-rowID,as.character),
   #       across(-rowID,~replace_na(.x,'')),
   #       across(-rowID,~case_when(
   #          .x=='' ~ 'no respons',
   #          .x %in% missing_values[2:3] ~ 'nonsubstantial',
   #          TRUE ~ 'respons'
   #          ))
   #       ) |>
   #    pivot_longer(-rowID,names_to='varname',values_to='behavior') |>
   #    left_join(tmp_variables,by='varname') |>
   #    group_by(rowID) |>
   #    mutate(
   #       n_socdem_resp = sum(
   #          behavior %in% c('respons','nonsubstantial') & part=='socdem'
   #          ),
   #       n_socdem = sum(part=='socdem'),
   #       p_socdem = n_socdem_resp/n_socdem,
   #       n_other_resp = sum(behavior %in% c('respons') & part=='other'),
   #       n_other = sum(part=='other'),
   #       p_other = n_other_resp/n_other
   #       ) |>
   #    filter(behavior %in% c('respons','nonsubstantial')) |>
   #    arrange(rowID,respqup) |>
   #    slice(n()) |>
   #    ungroup() |>
   #    select(rowID,p_socdem,p_other,respqup,respqudo)
   #
   # # maak respons variabelen aan
   # data_respons <- tmp_data |>
   #    left_join(tmp_dropout,join_by(rowID)) |>
   #    mutate(
   #       respqup=replace_na(respqup,0),
   #       respdisp=case_when(
   #          respdisp %in% c('Te laat','Gestorven','Niet bekwaam','Taalprobleem',
   #             'Verkeerde respondent','Onbereikbaar') ~ respdisp,
   #          p_socdem==1 & p_other>=.8 ~ 'Volledig antwoord',
   #          p_socdem==1 & p_other>=.3 ~ 'Partieel antwoord',
   #          respqup >0 ~ 'Break-off',
   #          respqup==0 ~ 'Weigering'
   #          ),
   #       respons = case_match(respdisp,
   #          c('Volledig antwoord','Partieel antwoord') ~ 'Respondent',
   #          .default='Nonrespondent'
   #          ),
   #       resptime=case_when(
   #           respmode=='Online' & zending1<=respdate & respdate<=zending2 ~ 'Na 1ste uitnodiging'
   #          ,respmode=='Online' & zending2< respdate & respdate<=zending3 ~ 'Na 1ste herinnering'
   #          ,respmode=='Online' & zending3< respdate                      ~ 'Na 2de herinnering'
   #          ,respmode=='Schriftelijk'                                     ~ 'Na 2de herinnering'
   #          )
   #       )
   #
   # # opkuisen
   # rm(list=ls(pattern="^tmp"))
   #
   #
   #
   #
   #
   #
   # # ----------------------------------------------------------------------
   # # STAP 7: VERWIJDER DUBBELS --------------------------------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> Verwijder dubbels\n')
   #
   # # neem steekproefgrootte
   # tmp_stkprfn <- pull(meta_edities,stkprfn)
   #
   # # Enkel dubbels berekenen als er dubbels zijn
   # if ( nrow(data_respons)==tmp_stkprfn ) {
   #    data_nodoubles <- mutate(data_respons,responsn=NA_real_,resptype='')
   # } else {
   #    data_nodoubles <- data_respons |>
   #       mutate(
   #          order=case_match(respmode,"Online"~1,"Schriftelijk"~2),
   #          priority=case_match(respons,'Respondent'~1,'Nonrespondent'~2),
   #          ) |>
   #       group_by(respid) |>
   #       arrange(respid,order,respdate) |>
   #       mutate(
   #          responsn=as.numeric(n()),
   #          resptype=str_c(respmode,collapse=' + ')
   #          ) |>
   #       arrange(respid,priority,desc(respqup),desc(respdate)) |>
   #       slice(1) |>
   #       ungroup() |>
   #       select(-priority)
   #    }
   #
   # # opkuisen
   # rm(list=ls(pattern="^tmp"))
   #
   #
   #
   #
   #
   #
   #
   # # ----------------------------------------------------------------------
   # # STAP 8: AFGELEIDE VARIABELEN -----------------------------------------
   # # ----------------------------------------------------------------------
   #

      #       resp_id=sprintf('%s-%04.f',srvyid,sample(1:n(),n())),
   #       natltygrp=case_when(
   #          natlty == 150              ~ 'Belg',
   #          natlty %in% tmp_eucountries ~ 'Niet-Belg EU',
   #          !is.na(natlty)              ~ 'Niet-Belg niet-EU'
   #          ),
   #       rrurbgr=type,


   # cat('-> Bereken afgeleide variabelen\n')
   #
   # # kopieer de dataset
   # data_derivedvar <- data_nodoubles
   #
   # # leeftijd
   # if ( 'srvyjr' %in% names(data_derivedvar) & 'rrgbrtjr' %in% names(data_derivedvar) ){
   #    data_derivedvar <- mutate(data_derivedvar,
   #       rrage = srvyjr - rrgbrtjr,
   #       rragect7 = cut(rrage,
   #          c(-Inf,17,24,34,44,54,64,74,Inf),
   #          c(NA,'18-24 jaar','25-34 jaar','35-44 jaar','45-54 jaar',
   #            '55-64 jaar','65-74 jaar','75 jaar en ouder')
   #          ) |> as.character(),
   #       rragect4 = cut(rrage,
   #          c(-Inf,17,34,49,64,Inf),
   #          c(NA,'18-34 jaar','35-49 jaar','50-64 jaar','65 jaar en ouder')
   #          ) |> as.character()
   #       )
   #    }
   #
   # # onderwijsniveau
   # if ( 'diploma' %in% names(data_derivedvar) ){
   #    data_derivedvar <- mutate(data_derivedvar,
   #       oplniv4 = case_match(diploma,
   #          'Geen' ~ 'Laaggeschoold',
   #          'Lager onderwijs' ~ 'Laaggeschoold',
   #          'Lager secundair onderwijs' ~ 'Laaggeschoold',
   #          'Hoger secundair onderwijs' ~ 'Middengeschoold',
   #          'Hoger niet-universitair onderwijs' ~  'Hooggeschoold',
   #          'Universitair onderwijs' ~ 'Hooggeschoold',
   #          'Een ander diploma' ~ 'Ander',
   #          'Ongeldig' ~NA,
   #          NA ~ NA,
   #          .default = 'error'
   #          ),
   #       oplniv3 = case_match(diploma,
   #          'Geen' ~ 'Laaggeschoold',
   #          'Lager onderwijs' ~ 'Laaggeschoold',
   #          'Lager secundair onderwijs' ~ 'Laaggeschoold',
   #          'Hoger secundair onderwijs' ~ 'Middengeschoold',
   #          'Hoger niet-universitair onderwijs' ~  'Hooggeschoold',
   #          'Universitair onderwijs' ~  'Hooggeschoold',
   #          'Een ander diploma' ~ NA,
   #          'Ongeldig' ~NA,
   #          NA ~ NA,
   #          .default = 'error'
   #          ),
   #       )
   #    }
   #
   # # professionele situatie
   # if ( 'profsit' %in% names(data_derivedvar) ){
   #    data_derivedvar <- mutate(data_derivedvar,
   #       profsitb = case_match(profsit,
   #          'Werkzoekend'~'Andere situatie',
   #          'Student of scholier'~'Andere situatie',
   #          'Huisvrouw of -man'~'Andere situatie',
   #          'Invaliditeit of arbeidsongeschiktheid'~'Andere situatie',
   #          'Een andere situatie'~'Andere situatie',
   #          .default = profsit
   #          )
   #       )
   #    }
   #
   # # huishoudpositie
   # if ( 'hh_all' %in% names(data_derivedvar) &
   #      'hh_par' %in% names(data_derivedvar) &
   #      'hh_kin' %in% names(data_derivedvar) &
   #      'hh_oud' %in% names(data_derivedvar) ){
   #    data_derivedvar <- mutate(data_derivedvar,
   #       tmp.ander=if_any(
   #          any_of(c('hh_par','hh_kin','hh_oud','hh_brzu','hh_af','hh_vri','hh_and','hh_all')),
   #          ~.x=='Ja'),
   #       hhpos=case_when(
   #          hh_all=='Ja' ~ 'Woont alleen',
   #          hh_par=='Ja'   & hh_kin=='Neen' ~ 'Woont met partner zonder kinderen',
   #          hh_par=='Ja'   & hh_kin=='Ja'   ~ 'Woont met partner en kind(eren)',
   #          hh_par=='Neen' & hh_kin=='Ja'   ~ 'Woont niet met partner wel met kind(eren)',
   #          hh_par=='Neen' & hh_oud=='Ja'   ~ 'Woont bij ouders',
   #          tmp.ander ~ 'Ander'
   #          ),
   #       hhposb = case_match(hhpos,
   #          'Woont alleen' ~ 'Woont alleen',
   #          'Woont met partner zonder kinderen' ~ 'Woont met partner zonder kinderen',
   #          'Woont met partner en kind(eren)' ~ 'Woont met partner en kind(eren)',
   #          'Woont niet met partner wel met kind(eren)' ~ 'Woont niet met partner wel met kind(eren)',
   #          'Woont bij ouders' ~ 'Ander',
   #          'Ander' ~ 'Ander',
   #          'Ongeldig' ~ 'Ongeldig',
   #          NA ~ NA,
   #          .default = 'error'
   #          ),
   #
   #       tmp.ander = NULL
   #       )
   #    }
   #
   # # totaal aantal internettoepassingen
   # tmp_vars <- c('int_comm','int_tewe','int_info','int_cove','int_bank',
   #               'int_aank','int_verk','int_game','int_muzi','int_lere',
   #               'int_nws','int_socm','int_eigm','int_rout')
   # tmp_vars_dich <- str_c(tmp_vars,'_dich')
   # if ( all( tmp_vars %in% names(data_derivedvar)) ) {
   #    data_derivedvar <-
   #       data_derivedvar |>
   #       mutate(
   #          across(all_of(tmp_vars),
   #             ~dichotomize(.x,'Ja',c('Neen','Niet van toepassing'),na=c('Weet niet/geen antwoord')),
   #             .names='{.col}_dich'
   #             ),
   #          inttotal = rowSums(across(all_of(tmp_vars_dich))),
   #          ) |>
   #       select(-all_of(tmp_vars_dich))
   #    }
   #
   # # totaal aantal politieke activeiten
   # tmp_vars <- c('pa_pet','pa_socm','pa_bet','pa_verg','pa_info','pa_cont',
   #               'pa_pers','pa_bew','pa_overl','pa_part','pa_and')
   # tmp_vars_dich <- str_c(tmp_vars,'_dich')
   # if ( all( tmp_vars %in% names(data_derivedvar)) ) {
   #    data_derivedvar <-
   #       data_derivedvar |>
   #       mutate(
   #          across(all_of(tmp_vars),
   #             ~dichotomize(.x,'Ja','Neen',na='Weet niet/geen antwoord'),
   #             .names='{.col}_dich'
   #             ),
   #          patotal = rowSums(across(all_of(tmp_vars_dich))),
   #          ) |>
   #       select(-all_of(tmp_vars_dich))
   #    }
   #
   # # lidmaatschap aantal soorten verenigingen
   # tmp_vars <- c('lid_sprt','lid_hbb','lid_knst','lid_sclt','lid_vkb','lid_brt','lid_schl',
   #               'lid_jgd','lid_oud','lid_soc','lid_mil','lid_polp','lid_and')
   # tmp_vars_dich <- str_c(tmp_vars,'_dich')
   # if ( all( tmp_vars %in% names(data_derivedvar)) ) {
   #    data_derivedvar <- data_derivedvar |>
   #       mutate(
   #          across(all_of(tmp_vars),
   #             ~dichotomize(.x,'Ja','Neen',na='Weet niet/geen antwoord'),
   #             .names='{.col}_dich'
   #             ),
   #          lidtotal   = rowSums(across(all_of(tmp_vars_dich))),
   #          ) |>
   #       select(-all_of(tmp_vars_dich))
   #    }
   #
   # # cultuurparticipatie
   # tmp_vars <- c('act_muz','act_bios','act_opra','act_dans','act_thtr','act_crcs','act_mscl','act_cbrt','act_musm','act_mnmt','act_bib')
   # if ( all( tmp_vars %in% names(data_derivedvar)) ) {
   #    tmp_recode_act <- tribble(
   #       ~val                           ,~wekelijks,~maandelijks,~participant,~antw,
   #       'Dagelijks of bijna dagelijks' ,TRUE      ,TRUE        ,TRUE        ,TRUE ,
   #       '1 of meerdere keren per week' ,TRUE      ,TRUE        ,TRUE        ,TRUE ,
   #       '1 of meerdere keren per maand',FALSE     ,TRUE        ,TRUE        ,TRUE ,
   #       '1 of meerdere keren per jaar' ,FALSE     ,FALSE       ,TRUE        ,TRUE ,
   #       'Nooit'                        ,FALSE     ,FALSE       ,FALSE       ,TRUE ,
   #       "Weet niet/geen antwoord"      ,NA        ,NA          ,NA          ,FALSE,
   #       'Ongeldig'                     ,NA        ,NA          ,NA          ,NA   ,
   #       )
   #    data_derivedvar <- data_derivedvar |>
   #       mutate(
   #          across(
   #             all_of(tmp_vars),
   #             ~recodevar(.x,pull(tmp_recode_act,wekelijks,val)),
   #             .names='tmp_w_{.col}'
   #             ),
   #          across(
   #             all_of(tmp_vars),
   #             ~recodevar(.x,pull(tmp_recode_act,maandelijks,val)),
   #             .names='tmp_m_{.col}'
   #             ),
   #          across(
   #             all_of(tmp_vars),
   #             ~recodevar(.x,pull(tmp_recode_act,participant,val)),
   #             .names='tmp_p_{.col}'
   #             ),
   #          across(
   #             all_of(tmp_vars),
   #             ~recodevar(.x,pull(tmp_recode_act,antw,val)),
   #             .names='tmp_a_{.col}'
   #             ),
   #          actparticip=case_when(
   #             rowSums(across(starts_with('tmp_p_act_')))>=1               ~'Ja',
   #             rowSums(across(starts_with('tmp_a_act_')))==length(tmp_vars)~'Neen',
   #             rowSums(across(starts_with('tmp_a_act_'))) <length(tmp_vars)~'Weet niet/geen antwoord',
   #             ),
   #          actkernpubl=case_when(
   #             rowSums(across(starts_with('tmp_w_act_')))>=1               ~'Ja',
   #             rowSums(across(starts_with('tmp_m_act_')))>=2               ~'Ja',
   #             rowSums(across(starts_with('tmp_a_act_')))==length(tmp_vars)~'Neen',
   #             rowSums(across(starts_with('tmp_a_act_'))) <length(tmp_vars)~'Weet niet/geen antwoord',
   #             ),
   #          actbelangstpart=case_when(
   #             actparticip=='Ja'                      & actkernpubl=='Ja'                      ~'Neen',
   #             actparticip=='Ja'                      & actkernpubl=='Neen'                    ~'Ja',
   #             actparticip=='Ja'                      & actkernpubl=='Weet niet/geen antwoord' ~'Weet niet/geen antwoord',
   #             actparticip=='Neen'                    & actkernpubl=='Neen'                    ~'Neen',
   #             actparticip=='Weet niet/geen antwoord' & actkernpubl=='Weet niet/geen antwoord' ~'Weet niet/geen antwoord',
   #             ),
   #          actnonpart=case_when(
   #             actparticip=='Ja'                      ~'Neen',
   #             actparticip=='Neen'                    ~'Ja',
   #             actparticip=='Weet niet/geen antwoord' ~'Weet niet/geen antwoord',
   #             ),
   #          ) |>
   #       select(
   #          -starts_with('tmp_w_act_'),
   #          -starts_with('tmp_m_act_'),
   #          -starts_with('tmp_p_act_'),
   #          -starts_with('tmp_a_act_')
   #          )
   #    }
   #
   # # opkuisen
   # rm(list=ls(pattern="^tmp"))
   #
   #
   #
   #
   #
   #
   #
   #
   # # ----------------------------------------------------------------------
   # # STAP 9: BEREKENING GEWICHTEN -----------------------------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> Bereken gewichten\n')
   # ### gebaseerd op https://www.r-bloggers.com/2018/12/survey-raking-an-illustration/
   #
   # # selecteer covariaten
   # tmp_covar <- c('rrgslcht','rragect7','rrnation','rrurban')
   #
   # # jaar van survey
   # tmp_srvyjr <-  pull(meta_edities,srvyjr)
   #
   # # pad van populatiedata
   # tmp_poppath <-
   #    '//WV162699/fs_kb_dkb/svr/4_Beveiligde_data/SV-bevragingen' |>
   #    file.path('02_Populatiedata/Output') |>
   #    file.path(str_glue('demobel_{tmp_srvyjr}.xlsx'))
   #
   # # hervorm de enquete gegevens voor de raking
   # tmp_survey <- data_derivedvar |>
   #    filter(respons=='Respondent') |>
   #    transmute(respid,across(all_of(tmp_covar),as.factor)) |>
   #    as.data.frame() |>
   #    na.omit()
   #
   # # laad populatie data
   # tmp_pop <- tmp_poppath |>
   #    readxl::excel_sheets() |>
   #    intersect(tmp_covar) |>
   #    set_names() |>
   #    map(readxl::read_xlsx,path=tmp_poppath) |>
   #    suppressWarnings() |>
   #    map(pull,p,1) |>
   #    map(~.x/sum(.x))
   #
   # # functie om cat() output te onderdrukken
   # quiet <- function(x) {
   #    sink(tempfile())
   #    on.exit(sink())
   #    invisible(force(x))
   #    }
   #
   # # bereken gewichten
   # tmp_weights <- tmp_pop |>
   #    anesrake::anesrake(
   #       dataframe = tmp_survey,
   #       caseid = tmp_survey$respid,
   #       cap=5,
   #       type='nolim'
   #       ) |>
   #    quiet()
   #
   # # bewaar informatie over weging
   # outputfolder |>
   #    file.path(str_glue("{the_surv_id}-informatie_weging.rds")) |>
   #    write_rds(x=tmp_weights)
   #
   # # koppel gewichten aan data
   # data_weights <- tmp_weights |>
   #    pluck('weightvec') |>
   #    enframe('respid','w') |>
   #    left_join(x=data_derivedvar,by='respid')
   #
   # # opkuisen
   # rm(list=ls(pattern="^tmp"))
   #
   #
   #
   #
   #
   #
   #
   # # ----------------------------------------------------------------------
   # # STAP 10: CHECK DATASET -----------------------------------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> Controleer gekuiste data\n')
   #
   # # Controleer aantal respondenten gekuiste data zoals in metadata
   # tmp_stkprfn <- pull(meta_edities,stkprfn)
   # tmp_nrow <- nrow(data_weights)
   # if ( tmp_stkprfn!=tmp_nrow) {
   #    str_glue('Aantal respondenten in data (n={tmp_nrow}) komt niet overeen
   #       met steekproefgrootte in metadata (n={tmp_stkprfn})') |>
   #       stop()
   #    }
   # rm(list=ls(pattern="^tmp"))
   #
   # # Controleer alle variabelen aanwezig in gekuiste data
   # tmp <-
   #    meta_variables |>
   #    filter(!varname %in% names(data_weights))
   # if ( nrow(tmp) > 0 ) {
   #    pull(tmp,varname) |>
   #    str_c(collapse=', ') |>
   #    sprintf(fmt='Variabelen %s ontbreken in de gecleande dataset.') |>
   #    str_c(' Cleaning programma of metadata moeten herbekeken worden.') |>
   #    stop()
   #    }
   # rm(list=ls(pattern="^tmp"))
   #
   # # Controleer class van de variabelen in gekuiste data
   # tmp1 <-
   #    meta_variables |>
   #    filter(varname %in% names(data_weights))
   # tmp2 <-
   #    data_weights |>
   #    select(all_of(tmp1$varname)) |>
   #    map(class) |>
   #    enframe(name='varname') |>
   #    left_join(tmp1,join_by(varname)) |>
   #    filter(!value==class)
   # if ( nrow(tmp2) > 0 ) {
   #    tmp2 |>
   #    mutate(message=str_glue('{varname}: {value} in plaats van {class}')) |>
   #    summarize(message=str_c(message,collapse='\n')) |>
   #    mutate(message=str_c(
   #       'Variabelen met verkeerde klasse in gekuiste data:\n',
   #       'Cleaning programma of metadata moeten herbekeken worden.\n',
   #       message)) |>
   #    pull(message) |>
   #    stop()
   #    }
   # rm(list=ls(pattern="^tmp"))
   #
   # # Controleer de waarden van schaalvariabelen in de steekproefdata
   # tmp1 <-
   #    meta_variables |>
   #    filter(varname %in% names(data_weights)) |>
   #    filter(!map_lgl(vallabels,is.null)) |>
   #    mutate(vallabels=map(vallabels,enframe)) |>
   #    unnest(vallabels)
   # tmp2 <-
   #    data_weights |>
   #    select(all_of(tmp1$varname)) |>
   #    select(where(is.character)) |>
   #    pivot_longer(everything(),names_to='varname') |>
   #    filter(!is.na(value)) |>
   #    distinct() |>
   #    anti_join(tmp1,join_by(varname,value))
   # if ( nrow(tmp2) > 0 ) {
   #    tmp2 |>
   #    mutate(message=str_glue('{varname}: "{value}"')) |>
   #    summarize(message=str_c(message,collapse='\n')) |>
   #    mutate(message=str_c(
   #       'Schaal variabelen met verkeerde waarden in gekuiste data:\n',
   #       'Cleaning programma of metadata moeten herbekeken worden.\n',
   #       message)) |>
   #    pull(message) |>
   #    stop()
   #    }
   # rm(list=ls(pattern="^tmp"))
   #
   # # Controleer de waarden van andere variabelen in de steekproefdata
   # tmp1 <-
   #    meta_variables |>
   #    filter(varname %in% names(data_weights)) |>
   #    filter(!is.na(format))
   # tmp2 <-
   #    data_weights |>
   #    select(all_of(tmp1$varname)) |>
   #    mutate(across(everything(),as.character)) |>
   #    pivot_longer(everything(),names_to='varname') |>
   #    distinct() |>
   #    left_join(tmp1,join_by(varname)) |>
   #    arrange(varname) |>
   #    filter(!str_detect(value,format))
   # if ( nrow(tmp2) > 0 ) {
   #    tmp2 |>
   #    mutate(message=str_glue('{varname}: waarde "{value}" volgt niet opgelegd format "{format}"')) |>
   #    summarize(message=str_c(message,collapse='\n')) |>
   #    mutate(message=str_c(
   #       'Schaal variabelen met verkeerde format in gekuiste data:\n',
   #       'Cleaning programma of metadata moeten herbekeken worden.\n',
   #       message)) |>
   #    pull(message) |>
   #    stop()
   #    }
   # rm(list=ls(pattern="^tmp"))
   #
   #
   #
   #
   # # ----------------------------------------------------------------------
   # # STAP 11: Schrijf data weg --------------------------------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> Schrijf gekuiste data weg\n')
   #
   # # maak data om weg te schrijven
   # tmp_var <- pull(meta_variables,varname)
   # data_out <- data_weights |>
   #    select(all_of(tmp_var))
   #
   # # schrijf data weg
   # data_out |>
   #    group_by(srvyid) |>
   #    arrow::write_dataset(
   #       path=r"{\\WV162699\fs_kb_dkb\svr\4_Beveiligde_data\SV-bevragingen\04_Edities_cleandata\01_repondentdata}",
   #       format="parquet"
   #       )
   #
   # rm(list=ls(pattern="^tmp"))
   # rm(data_merged,data_inconsist,data_cata,data_filter,data_respons,data_nodoubles,data_derivedvar,data_weights)
   #
   #
   #
   #
   # # ----------------------------------------------------------------------
   # # FINAL: einde functie -------------------------------------------------
   # # ----------------------------------------------------------------------
   #
   # # return data
   # cat('Cleaning succesvol voltooid!\n')
   # invisible(data_out)


   }