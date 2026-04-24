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
   # STAP 1A: CHECK SAMPLE DATA -------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> check sample data\n')

   meta_variables_sample <- SVsurvey_read_sample_variables(the_surv_id)

   meta_dsd_sample <-
      SVsurvey_load_dsd(meta_variables_sample$concept_id) |>
      dplyr::rename(new_concept_id=concept_id) |>
      dplyr::left_join(
         rename(meta_variables_sample,new_concept_id=concept_id,concept_id=variable),
         dplyr::join_by(new_concept_id)
         )

   data_sample <- data_sample |>
      validate_data(meta_dsd_sample,silent=TRUE) |>
      rename(!!!pull(meta_variables_sample,variable,concept_id))


   # ----------------------------------------------------------------------
   # STAP 1B: CHECK SURVEY DATA -------------------------------------------
   # ----------------------------------------------------------------------

   cat('-> check survey data\n')

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

   meta_variables_survey <- SVsurvey_read_survey_variables(the_surv_id)

   meta_dsd_survey <-
      SVsurvey_load_dsd(meta_variables_survey$concept_id) |>
      dplyr::rename(new_concept_id=concept_id) |>
      dplyr::left_join(
         rename(meta_variables_survey,new_concept_id=concept_id,concept_id=variable),
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
      dplyr::rename(!!!pull(meta_variables_survey,variable,concept_id))

   rm(list=ls(pattern="^tmp"))




   # # ----------------------------------------------------------------------
   # # STAP 1: CONTROLEER DE INPUT DATA -------------------------------------
   # # ----------------------------------------------------------------------
   #

   # # STAP 1B: CONTROLEER VARIABELEN ENQUETE DATA --------------------------
    #
   # # controleer respons variabelen in enquetedata
   # tmp_respdisp   <- meta_variables |> filter(varname=='respdisp') |> pull(origvarname)
   # tmp_respmode   <- meta_variables |> filter(varname=='respmode') |> pull(origvarname)
   # tmp_respqrur   <- meta_variables |> filter(varname=='respqrur') |> pull(origvarname)
   # tmp_respdevice <- meta_variables |> filter(varname=='respdevice') |> pull(origvarname)
   # tmp_respdate   <- meta_variables |> filter(varname=='respdate') |> pull(origvarname)
   # tmp <- survey |>
   #    filter(
   #       !!sym(tmp_respdisp) %in% c('Volledig antwoord','Partieel antwoord','Break-off') &
   #       is.na(!!sym(tmp_respmode))
   #       )
   # if ( nrow(tmp)>0 ) {
   #    errors <- str_c(errors,'\nEr zijn respondenten met missende respons modus.\n\n')
   #    }
   # tmp <- survey |>
   #    filter(
   #       !!sym(tmp_respmode) == 'Online' &
   #       is.na(!!sym(tmp_respqrur))
   #       )
   # if ( nrow(tmp)>0 ) {
   #    errors <- str_c(errors,'\nEr zijn online respondenten met missende informatie over QR/URL.\n\n')
   #    }
   # tmp <- survey |>
   #    filter(
   #       !!sym(tmp_respmode) == 'Online' &
   #       is.na(!!sym(tmp_respdate))
   #       )
   # if ( nrow(tmp)>0 ) {
   #    errors <- str_c(errors,'\nEr zijn online respondenten met missende starttijd.\n\n')
   #    }
   # if ( 'respdevice' %in% meta_variables$varname ){
   #    tmp <- survey |>
   #       filter(
   #          !!sym(tmp_respmode) == 'Online' &
   #          is.na(!!sym(tmp_respdevice))
   #       )
   #    if ( nrow(tmp)>0 ) {
   #       errors <- str_c(errors,'\nEr zijn online respondenten met missende device.\n\n')
   #       }
   #    }
   #
   #
   # #### TOEVOEGEN: CHECK DAT ER NIET TE VEEL VARIABELEN IN ZITTEN. Bij SV10 zat
   # #### variabele srvyjr erin wat problemen opleverde bij merge met metadata.
   #
   # #### Check of alle originele variabelen er in zitten. in SV10 zat variabele
   # #### V6_99 maar in het codeboek stond V6_9
   #
   #
   # #### Check dat starttijd enkel is ingevuld als methodiek=='Online'
   #
   #
   #
   # # STAP 1C: THROW ERRORS ------------------------------------------------
   #
   # if ( !errors=='' ) {
   #    write(errors,'errors.txt')
   #    shell.exec("errors.txt")
   #    stop('Data nog niet in orde!')
   #    }
   # rm(errors)
   #
   #
   #
   #
   #
   # # ----------------------------------------------------------------------
   # # STAP 2: CLEAN AND MERGE DIFFERENT DATA SOURCES -----------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> Kuis en voeg verschillende databronnen samen\n')
   #
   # # Laad editie metadata
   # meta_edities <-
   #    '//WV162699/fs_kb_dkb/svr/4_Beveiligde_data/SV-bevragingen/' |>
   #    file.path('01_Metadata/edities.xlsx') |>
   #    readxl::read_xlsx() |>
   #    suppressWarnings() |>
   #    filter(srvyid==the_surv_id) |>
   #    transmute(
   #       srvyid=srvyid,
   #       vwperiod=vwperiod,
   #       srvyjr=as.numeric(srvyjr),
   #       srvyprjr=str_c(str_to_sentence(srvypr),' ',srvyjr),
   #       stkprfn=as.numeric(stkprfn),
   #       )
   #
   # # laad zendingen metadata
   # tmp_recoding_zendingen <- c(
   #    'uitnodiging'     ='zending1',
   #    '1ste herinnering'='zending2',
   #    '2de herinnering' ='zending3'
   #    )
   # tmp_zendingen <-
   #    '//WV162699/fs_kb_dkb/svr/4_Beveiligde_data/SV-bevragingen/' |>
   #    file.path('01_Metadata/zendingen.xlsx') |>
   #    readxl::read_xlsx() |>
   #    suppressWarnings() |>
   #    filter(!is.na(vwperiod)) |>
   #    transmute(
   #       vwperiod,
   #       zending=tmp_recoding_zendingen[zending],
   #       datum=datum,
   #       ) |>
   #    filter(!is.na(zending)) |>
   #    pivot_wider(id_cols=vwperiod,names_from=zending,values_from=datum)
   #
   #
   # # Laad urbanisatie data
   # tmp_urbanisatie <-
   #    r'{\\WV162699\fs_kb_dkb\svr\4_Beveiligde_data\SV-bevragingen\01_Metadata}' |>
   #    file.path('NISdata.csv') |>
   #    read_csv2(
   #       col_types=c(CD_REFNIS='c',TX_PROV_DESCR_NL='c',urbanc='c',.default='-')
   #       ) |>
   #    suppressMessages() |>
   #    transmute(
   #       rrnis  = as.numeric(CD_REFNIS),
   #       rrprovin = TX_PROV_DESCR_NL %>% str_replace('^Provincie ',''),
   #       rrurban  = str_to_sentence(urbanc),
   #       )
   #
   # # lijst codes EU landen
   # tmp_eucountries <- 8000 +
   #    c(106,10,108,103,136,11,111,114,115,116,128
   #    ,146,135,13,113,119,129,105,139,123,124,147,141,109,140,126)
   #
   # # zet seed obv surveynaam voor willekeurige toewijzing respondent ID's
   # set.seed(parse_number(the_surv_id)+1213)
   # tmp_sample <- sample |>
   #    transmute(
   #       pseudoid,
   #       rrnis  = as.numeric(cd_refnis),
   #       srvyid=the_surv_id,
   #       respid=sprintf('%s-%04.f',srvyid,sample(1:n(),n())),
   #       rrgslcht=str_to_sentence(geslacht),
   #       rrgbrtjr=jaar_bth,
   #       rrgbrtmd=c('januari','februari','maart','april','mei','juni','juli',
   #          'augustus','september','oktober','november','december')[maand_bth],
   #       rrnation=case_when(
   #          cd_nat == 8150              ~ 'Belg',
   #          cd_nat %in% tmp_eucountries ~ 'Niet-Belg EU',
   #          !is.na(cd_nat)              ~ 'Niet-Belg niet-EU'
   #          ),
   #       rrurbgr=type,
   #       )
   #
   # # vector met mapping naar nieuwe variabele namen
   # tmp_newnames <- meta_variables |>
   #    filter(origvarname %in% names(survey)) |>
   #    pull(origvarname,varname)
   #
   # # kuis enquetedata
   # tmp_survey <- survey |>
   #    rename(!!!tmp_newnames)
   #
   # # merge all data
   # data_merged <- tmp_sample |>
   #    left_join(tmp_survey,join_by(pseudoid)) |>
   #    left_join(meta_edities,join_by(srvyid),suffix=c(".x","")) |>
   #    left_join(tmp_zendingen,join_by(vwperiod),suffix=c(".x","")) |>
   #    left_join(tmp_urbanisatie,join_by(rrnis),suffix=c(".x",""))
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
   #
   #
   # # ----------------------------------------------------------------------
   # # STAP 3: CORRECTIE INCONSISTENTIES ------------------------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> Corrigeer inconsistenties\n')
   #
   # data_inconsist <- data_merged
   #
   # # corrigeer geboortejaar, leeftijd moet tussen 18 en 120 liggen
   # if ( "gebjaar" %in% names(data_inconsist) ) {
   #    data_inconsist <- mutate(data_inconsist,
   #       tmp_lftd=srvyjr-gebjaar-1,
   #       gebjaar = case_when(
   #          tmp_lftd<18 | tmp_lftd>120 ~ NA,
   #          TRUE ~ gebjaar
   #          ),
   #       tmp_lftd=NULL,
   #       )
   #    }
   #
   # # nhh_knd naar nul als vragen over personen in huishouden zijn beantwoord
   # # en nhh_knd leeg is.
   # if ( any(str_detect(names(data_inconsist),'^hh_[a-z]+$')) &
   #      'nhh_knd' %in% names(data_inconsist)                 ) {
   #    data_inconsist <- mutate(data_inconsist,
   #       tmp_hh=if_any(matches('^hh_[a-z]+$'),~.x=='Ja'),
   #       nhh_knd=case_when(
   #          tmp_hh & is.na(nhh_knd) ~ 0,
   #          TRUE ~ nhh_knd
   #          ),
   #       tmp_hh = NULL,
   #       )
   #    }
   #
   # # nhh_vlw naar nul als vragen over personen in huishouden zijn beantwoord
   # # en nhh_vlw leeg is.
   # if ( any(str_detect(names(data_inconsist),'^hh_[a-z]+$')) &
   #      'nhh_vlw' %in% names(data_inconsist)                 ) {
   #    data_inconsist <- mutate(data_inconsist,
   #       tmp_hh=if_any(matches('^hh_[a-z]+$'),~.x=='Ja'),
   #       nhh_vlw=case_when(
   #          tmp_hh & is.na(nhh_vlw) ~ 0,
   #          TRUE ~ nhh_vlw
   #          ),
   #       tmp_hh = NULL,
   #       )
   #    }
   #
   # # nhh_sen naar nul als vragen over personen in huishouden zijn beantwoord
   # # en nhh_sen leeg is.
   # if ( any(str_detect(names(data_inconsist),'^hh_[a-z]+$')) &
   #      'nhh_sen' %in% names(data_inconsist)                 ) {
   #    data_inconsist <- mutate(data_inconsist,
   #       tmp_hh=if_any(matches('^hh_[a-z]+$'),~.x=='Ja'),
   #       nhh_sen=case_when(
   #          tmp_hh & is.na(nhh_sen) ~ 0,
   #          TRUE ~ nhh_sen
   #          ),
   #       tmp_hh = NULL,
   #       )
   #    }
   #
   # # hh_kind='Ja' als nhh_kind>0
   # if ( 'hh_kin' %in% names(data_inconsist)  &
   #      'nhh_knd' %in% names(data_inconsist) ) {
   #    data_inconsist <- mutate(data_inconsist,
   #       hh_kin = case_when(
   #          nhh_knd>0 ~ 'Ja',
   #          TRUE ~ hh_kin
   #          ),
   #       )
   #    }
   #
   # # hh_all='Neen' als er personen in huishouden worden gesignaleerd
   # if ( 'hh_all' %in% names(data_inconsist)  &
   #      all(c('nhh_knd','nhh_vlw','nhh_sen') %in% names(data_inconsist)) ) {
   #    data_inconsist <- mutate(data_inconsist,
   #       hh_all = case_when(
   #          nhh_knd>0 | nhh_vlw>0 | nhh_sen>0 ~ 'Neen',
   #          TRUE ~ hh_all
   #          ),
   #       )
   #    }
   #
   #
   #
   #
   #
   #
   # # ----------------------------------------------------------------------
   # # STAP 4: CLEAN CHECK-ALL-THAT-APPLY QUESTIONS -------------------------
   # # ----------------------------------------------------------------------
   #
   # cat('-> Kuis check-all-that-apply variabelen\n')
   #
   # # function for cleaning check-all-that-apply questions without 'none'
   # tmp_cleancheckallapply_withoutnone <- function(.data,.var,.dk) {
   #    mutate(.data,
   #       across(any_of(c(.var,.dk)),~na_if(.x,'Neen')),
   #       tmp_any=if_any(any_of(.var),~.x=='Ja' ),
   #       across(any_of(.var),~case_when(
   #          .x=='Ja'~'Ja',
   #          !!sym(.dk)=='Ja'~'Weet niet/geen antwoord',
   #          tmp_any~'Neen'
   #          )),
   #       {{.dk}} := case_when(
   #          !!sym(.dk)=='Ja'~'Ja',
   #          tmp_any~'Neen'
   #          ),
   #       tmp_any=NULL,
   #       )
   #    }
   # # crossing(v1=c('Ja',NA),v2=c('Ja',NA),dk=c('Ja',NA)) |>
   # #    mutate(across(everything(),~.x,.names='{.col}_old'),.before=1) |>
   # #    tmp_cleancheckallapply_withoutnone(.var=c('v1','v2'),.dk='dk') |>
   # #    View()
   #
   # # function for cleaning check-all-that-apply questions with 'none'
   # tmp_cleancheckallapply_withnone <- function(.data,.var,.dk,.none) {
   #    mutate(.data,
   #       across(any_of(c(.var,.dk,.none)),~na_if(.x,'Neen')),
   #       tmp_any  = if_any(any_of(.var),~.x=='Ja' ),
   #       across(any_of(.var),~case_when(
   #          .x=='Ja'~'Ja',
   #          !!sym(.dk)=='Ja' ~'Weet niet/geen antwoord',
   #          !!sym(.none)=='Ja' ~'Neen',
   #          tmp_any~'Neen'
   #          )),
   #       {{.none}} := case_when(
   #          tmp_any~'Neen',
   #          !!sym(.dk)=='Ja'~'Weet niet/geen antwoord',
   #          !!sym(.none)=='Ja'~'Ja'
   #          ),
   #       {{.dk}} := case_when(
   #          !!sym(.dk)=='Ja'~'Ja',
   #          tmp_any~'Neen',
   #          !!sym(.none)=='Ja'~'Neen'
   #          ),
   #       tmp_any=NULL,
   #       tmp_any2=NULL,
   #       )
   #    }
   # # crossing(v1=c('Ja',NA),v2=c('Ja',NA),none=c('Ja',NA),dk=c('Ja',NA)) |>
   # #    mutate(across(everything(),~.x,.names='{.col}_old'),.before=1) |>
   # #    tmp_cleancheckallapply_withnone(.var=c('v1','v2'),.dk='dk',.none='none') |>
   # #    View()
   #
   # # generic function for cleaning check-all-that-apply questions
   # tmp_cleancheckallapply <- function(.data,.var,.dk,.none='') {
   #    if ( any(.var %in% names(.data)) & .dk %in% names(.data) & .none=='' ) {
   #       .data <- tmp_cleancheckallapply_withoutnone(.data,.var,.dk)
   #       }
   #    if ( any(.var %in% names(.data)) &
   #         .dk %in% names(.data) &
   #         .none %in% names(.data) ) {
   #       .data <- tmp_cleancheckallapply_withnone(.data,.var,.dk,.none)
   #       }
   #    return(.data)
   #    }
   #
   # # kuis de check-all-that-apply vragen
   # data_cata  <- data_inconsist |>
   #    tmp_cleancheckallapply(
   #       .var=c('hh_par','hh_kin','hh_oud','hh_brzu','hh_af','hh_vri','hh_and'),
   #       .dk='hh_wn',
   #       .none='hh_all'
   #       ) |>
   #    tmp_cleancheckallapply(
   #       .var=c('lid_sprt','lid_hbb' ,'lid_knst','lid_sclt','lid_vkb',
   #              'lid_brt' ,'lid_schl','lid_jgd' ,'lid_oud' ,'lid_soc',
   #              'lid_mil','lid_polp' ,'lid_and'),
   #       .dk='lid_wn',
   #       .none='lid_geen'
   #       ) |>
   #    tmp_cleancheckallapply(
   #       .var=c('sprt_wa' ,'sprt_fts','sprt_lop','sprt_ftn','sprt_zwm',
   #              'sprt_vtb','sprt_vlb','sprt_bkb','sprt_tns','sprt_pdl',
   #              'sprt_yga','sprt_gvs','sprt_dns','sprt_trn','sprt_and'),
   #       .dk='sprt_wn'
   #       ) |>
   #    tmp_cleancheckallapply(
   #       .var=c('mb_comp','mb_tabl','mb_spho','mb_gsm',
   #              'mb_stv','mb_tv','mb_rad'),
   #       .dk='mb_wn',
   #       .none='mb_geen'
   #       ) |>
   #    tmp_cleancheckallapply(
   #       .var=c('tbd_dtv','tbd_smov','tbd_smuz','tbd_abkr','tbd_abma'),
   #       .dk='tbd_wn',
   #       .none='tbd_geen'
   #       ) |>
   #    tmp_cleancheckallapply(
   #       .var=c('pa_pet','pa_socm','pa_bet','pa_verg','pa_info','pa_cont',
   #                       'pa_pers','pa_bew','pa_overl','pa_part','pa_and'),
   #       .dk='pa_wn',
   #       .none='pa_geen'
   #       ) |>
   #    tmp_cleancheckallapply(
   #       .var=c('intv_vst','intv_mob'),
   #       .dk='intv_wn'
   #       ) |>
   #    tmp_cleancheckallapply(
   #       .var=c('int_comm','int_tewe','int_info','int_cove','int_bank',
   #              'int_aank','int_verk','int_game','int_muzi','int_lere',
   #              'int_nws','int_socm','int_eigm','int_rout'),
   #       .dk='int_wn'
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
   #
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