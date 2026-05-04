rm(list=ls())
devtools::load_all("00_functions")

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

tmp_correct_varlabels <- c(
	"Ingevuld"="Responsdispositie", # instead of "Ingevuld"
	"Starttijd"="Datum van respons", # instead of "Starttijd"
	"Methodiek"="Respons modus", # instead of "Werd de enquête schriftelijk geretourneerd of via internet?"
	"QRcode"="Response via QR of URL", # instead of "Ben je op deze vragenlijst terecht gekomen via een QR-code of niet?"
	"V1"="Geslacht", # instead of "Wat is uw geslacht?"
	"V2_1"="Geboortejaar", # instead of "In welk jaar en welke maand bent u geboren? JAAR"
	"V2_2"="Geboortemaand", # instead of "In welk jaar en welke maand bent u geboren? MAAND"
	"V3"="Professionele situatie", # instead of "In welke situatie bevindt u zich momenteel? "
	"V3a"="Professionele situatie anders", # instead of "Anders (omschrijf) :"
	"V4"="Diploma", # instead of "Wat is het hoogste diploma dat u heeft behaald? "
	"V4a"="Diploma", # instead of "Anders (omschrijf):"
	"V5_1"="Huishouden met partner", # instead of "[Met een partner] Met welke mensen leeft u samen in uw woning?    Meerdere antwoorden mogelijk"
	"V5_2"="Huishouden met kinderen", # instead of "[Met kinderen (ook in co-ouderschap, ook volwassen kinderen)] Met welke mensen leeft u samen in uw woning?    Meerdere antwoorden mogelijk"
	"V5_3"="Huishouden met (schoon)ouders", # instead of "[Met ouder(s) ] Met welke mensen leeft u samen in uw woning?    Meerdere antwoorden mogelijk"
	"V5_4"="Huishouden met andere familie", # instead of "[Met andere familieleden] Met welke mensen leeft u samen in uw woning?    Meerdere antwoorden mogelijk"
	"V5_5"="Huishouden met andere personen", # instead of "[Met andere, niet familie (omschrijf) :] Met welke mensen leeft u samen in uw woning?    Meerdere antwoorden mogelijk"
	"V5_6"="Huishouden alleen", # instead of "[Ik woon alleen ] Met welke mensen leeft u samen in uw woning?    Meerdere antwoorden mogelijk"
	"V5_99"="Huishouden met WN/GA", # instead of "[Weet niet/geen antwoord] Met welke mensen leeft u samen in uw woning?    Meerdere antwoorden mogelijk"
	"V5a"="Huishouden met andere", # instead of "Met andere, niet familie (omschrijf) :"
	"V6"="Woonomgeving", # instead of "Hoe omschrijft u uw woonomgeving?"
	"V6a"="Woonomgeving andere", # instead of "Ander (omschrijf) :"
	"V7"="Subjectief inkomen", # instead of "In welke mate kan uw gezin rondkomen met het beschikbare inkomen?"
	"V8"="Gevoel onveilig in buurt", # instead of "Hoe vaak voelt u zich onveilig in uw buurt of wijk?"
	"V9"="Mijding plekken in de gemeenten", # instead of "Hoe vaak mijdt u bepaalde plekken in uw gemeente of stad omdat u het er niet veilig vindt?"
	"V10"="Contact familie", # instead of "Hoe dikwijls ontmoet u niet-inwonende familie?"
	"V11"="Contact vrienden", # instead of "Hoe dikwijls ontmoet u vrienden of kennissen?"
	"V12"="Contact buren", # instead of "Hoe dikwijls ontmoet u buren?"
	"V13"="Informele zorg", # instead of "Hoe vaak heeft u zelf de afgelopen 12 maanden zieke, gehandicapte of bejaarde familieleden, kennissen of buren geholpen of verzorgd?"
	"V14"="Vlaamse overheid klantgericht", # instead of "‘De Vlaamse overheid is klantgericht’"
	"V15"="Vlaamse overheid transparant", # instead of "’De Vlaamse overheid is transparant’"
	"V16"="Vlaamse overheid efficient", # instead of "‘De Vlaamse overheid is efficiënt’"
	"V17"="Vlaamse overheid vernieuwend", # instead of "‘De Vlaamse overheid is vernieuwend’"
	"V18"="Vlaamse overheid toegankelijk", # instead of "‘De Vlaamse overheid is toegankelijk'"
	"V19"="Tevredenheid beleid lokale overheid", # instead of "Hoe tevreden bent u in het algemeen over het beleid van uw lokale overheid?  U kan antwoorden met een score van 0 (heel ontevreden) tot 10 (heel tevreden)."
	"V20"="Tevredenheid beleid provinciale overheid", # instead of "Hoe tevreden bent u in het algemeen over het beleid van uw provinciale overheid?  U kan antwoorden met een score van 0 (heel ontevreden) tot 10 (heel tevreden)."
	"V21"="Tevredenheid beleid Vlaamse overheid", # instead of "Hoe tevreden bent u in het algemeen over het beleid van de Vlaamse overheid?   U kan antwoorden met een score van 0 (heel ontevreden) tot 10 (heel tevreden)."
	"V22"="Tevredenheid beleid federale overheid", # instead of "Hoe tevreden bent u in het algemeen over het beleid van de federale (Belgische) overheid?   U kan antwoorden met een score van 0 (heel ontevreden) tot 10 (heel tevreden)."
	"V23"="Tevredenheid beleid Europese overheid", # instead of "Hoe tevreden bent u in het algemeen over het beleid van de Europese overheid?   U kan antwoorden met een score van 0 (heel ontevreden) tot 10 (heel tevreden)."
	"V24"="Vlaamse overheid geeft voldoende info", # instead of "‘De Vlaamse overheid geeft voldoende informatie’"
	"V25"="Vlaamse overheid geeft tijdig info", # instead of "‘De Vlaamse overheid geeft tijdig informatie’"
	"V26"="Vlaamse overheid geeft correcte info", # instead of "‘De Vlaamse overheid geeft correcte informatie’"
	"V27"="Vlaamse overheid is duidelijk", # instead of "‘De Vlaamse overheid is duidelijk’"
	"V28"="Vertrouwen lokale overheid", # instead of "Hoeveel vertrouwen heeft u in uw lokale overheid?"
	"V29"="Vertrouwen provinciale overheid", # instead of "Hoeveel vertrouwen heeft u in uw provinciale overheid?"
	"V30"="Vertrouwen Vlaamse overheid", # instead of "Hoeveel vertrouwen heeft u in de Vlaamse overheid?"
	"V31"="Vertrouwen federale overheid", # instead of "Hoeveel vertrouwen heeft u in de federale (Belgische) overheid?"
	"V32"="Vertrouwen Europese overheid", # instead of "Hoeveel vertrouwen heeft u in de Europese overheid?"
	"V33"="Medemens is te vertrouwen", # instead of "Denkt u dat u niet voorzichtig genoeg kan zijn in de omgang met mensen of dat de meeste mensen te vertrouwen zijn?  U kan antwoorden met een score van :  0    = men kan niet voorzichtig genoeg zijn in de omgang met mensen,  10  = de meeste mensen zijn"
	"V34"="Medemens maakt misbruik of is eerlijk", # instead of "Denkt u dat de meeste mensen zouden proberen misbruik van u te maken als zij daartoe de kans krijgen of dat zij eerlijk proberen te zijn?  U kan antwoorden met een score van :  0    = de meeste mensen zouden proberen misbruik van mij te maken als zij da"
	"V35"="Medemens is zelfzuchtig of behulpzaam", # instead of "Denkt u dat de meeste mensen meestal aan zichzelf denken of dat zij meestal behulpzaam proberen te zijn?  U kan antwoorden met een score van :  0    = mensen denken meestal aan zichzelf,  10  = mensen proberen meestal behulpzaam te zijn"
	"V36_fieldcode"="Verdere opmerkingen"  # instead of "Indien u nog opmerkingen kwijt wil, dan kan u deze hieronder noteren."
   )

data_survey <-
   r'{..\02_sourcedata\SV0001\02_survey\SV - Datafile - Finaal.sav}' |>
   haven::read_sav() |>
   dplyr::left_join(tmp_idlink,dplyr::join_by(Respondentnummer)) |>
   coalesce_join(tmp_recodes,dplyr::join_by(Respondentnummer)) |>
   SVsurvey_labelled_2_tibble(the_surv_id,tmp_correct_varlabels) |>
   dplyr::select(-Respondentnummer,-Surveyjaar,-Dubbel,-ID_dubbel,-Eindtijd,-Exportfile,-PricavyInfo) |>
   dplyr::mutate(
      Starttijd = lubridate::as_date(Starttijd),
      V2_1 = dplyr::replace_values(V2_1,1534~1934,4307~NA),
      V2_2 = dplyr::replace_values(V2_2,21~12),
      Ingevuld = dplyr::replace_values(Ingevuld,
         "Ja (minstens 1 vraag)"~"Antwoord",
         "Neen, blanco / geen medewerking"~"Weigering",
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




