rm(list=ls())
devtools::load_all("00_functions")

# library(tidyverse)

the_surv_id <- 'SV0001'



# Load sample data --------------------------------------------------------

data_sample <-
   r"{..\02_sourcedata\%s\01_sample}" |>
   sprintf(the_surv_id) |>
   file.path("SAMP_VSA_SV_THEMATIQUE.xlsx") |>
   readxl::read_xlsx() |>
   dplyr::select(-CD_ZIP_RES,-gemeente,-geslacht,-CNTRY_BTH,-CNTRY_NAT) |>
   dplyr::mutate(
      CD_CNTRY_BTH=tidyr::replace_na(CD_CNTRY_BTH,"999"),
      CD_SEX=as.numeric(CD_SEX),
      CD_CNTRY_BTH=dplyr::replace_values(CD_CNTRY_BTH,"134"~"999"),
      )


# Load survey data --------------------------------------------------------

tmp_idlink <-
   r"{..\02_sourcedata\%s\02_survey}" |>
   sprintf(the_surv_id) |>
   file.path("Pseudoids_sv (voor link gerealiseerde en geplande, deel SV).xlsx") |>
   readxl::read_xlsx() |>
   dplyr::select(Respondentnummer=idfinaal,pseudo_id=`pseudo-id`)

tmp_recodes <-
   r"{03_process_editions\%s\input\%s_hercodering_obv_tekstvakken.xlsx}" |>
   sprintf(the_surv_id,the_surv_id) |>
   readxl::read_xlsx() |>
   tidyr::pivot_wider(id_cols=Respondentnummer,names_from=variable,values_from=value)

tmp_varlabels <-
   SVsurvey_read_survey_variables(the_surv_id) |>
   dplyr::pull(variable_label,variable)

data_survey <-
   r'{..\02_sourcedata\SV0001\02_survey\SV - Datafile - Finaal.sav}' |>
   haven::read_sav() |>
   dplyr::left_join(tmp_idlink,dplyr::join_by(Respondentnummer)) |>
   coalesce_join(tmp_recodes,dplyr::join_by(Respondentnummer)) |>
   labelled_2_tibble(check_varlabels=tmp_varlabels) |>
   dplyr::select(-Respondentnummer,-Surveyjaar,-Dubbel,-ID_dubbel,-Eindtijd,-Exportfile,-PricavyInfo) |>
   dplyr::mutate(
      V2_1 = dplyr::replace_values(V2_1,1534~1934,4307~NA),
      V2_2 = dplyr::replace_values(V2_2,21~12),
      Ingevuld = dplyr::replace_values(Ingevuld,
         "2.31"~'Gestorven',
         "2.32"~'Incapabel',
         "2.36"~'Verkeerde respondent'
         ),
    Methodiek = dplyr::replace_values(Methodiek,
      "online"~'Online',
      "schriftelijke vragenlijst"~'Schriftelijk',
      ),
    QRcode = dplyr::case_when(
      QRcode=="Ja, via een QR-code"~'QR',
      Methodiek=='Online'~'URL'
      ),
    dplyr::across(
      tidyselect::where(is.character),
      \(x) dplyr::replace_values(x,
        'Meerdere antwoorden'~'Ongeldig',
        'Dubbel antwoord'~'Ongeldig',
        'Geen opgave'~NA,
        'Niet geselecteerd'~'Neen',
        "weet niet/geen antwoord"~'Weet niet/geen antwoord',
        )
      ),
    dplyr::across(V19|V20|V21|V22|V23,
      \(x) dplyr::replace_values(x,
        "0"~'0: heel ontevreden',
        "10"~'10: heel tevreden',
        )
      ),
    V33 = dplyr::replace_values(V33,
      '0' ~'0: Men kan niet voorzichtig genoeg zijn in de omgang met mensen',
      '10'~'10: De meeste mensen zijn te vertrouwen',
      '11'~'10: De meeste mensen zijn te vertrouwen'
      ),
    V34 = dplyr::replace_values(V34,
      '0' ~'0: De meeste mensen zouden proberen misbruik van mij te maken als zij daartoe de kans krijgen',
      '10'~'10: De meeste mensen proberen eerlijk te zijn',
      '11'~'10: De meeste mensen proberen eerlijk te zijn'
      ),
    V35 = dplyr::replace_values(V35,
      '0' ~'0: Mensen denken meestal aan zichzelf',
      '10'~'10: Mensen proberen meestal behulpzaam te zijn',
      '11'~'10: Mensen proberen meestal behulpzaam te zijn'
      ),
    )

rm(list=ls(pattern="^tmp"))


# -------------------------------------------------------------------------
# Gestructureerde cleaning ------------------------------------------------
# -------------------------------------------------------------------------

data <- SVsurvey_clean_data(
   the_surv_id=the_surv_id,
   data_sample=data_sample,
   data_survey=data_survey
   )




