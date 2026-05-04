rm(list=ls())
devtools::load_all("00_functions")

the_surv_id <- 'SV0002'



# Load sample data --------------------------------------------------------

data_sample <-
   r"{..\02_sourcedata\%s\01_sample}" |>
   sprintf(the_surv_id) |>
   file.path("SV2_steekproef.xlsx") |>
   readxl::read_xlsx() |>
   dplyr::select(-CD_ZIP_RES,-gemeente,-geslacht,-CNTRY_BTH,-CNTRY_NAT,-survey_num) |>
   dplyr::mutate(
      CD_SEX=as.numeric(CD_SEX),
      )


# Load survey data --------------------------------------------------------

tmp_recodes <-
   r"{03_process_editions\%s\input\%s_hercodering_obv_tekstvakken.xlsx}" |>
   sprintf(the_surv_id,the_surv_id) |>
   readxl::read_xlsx() |>
   tidyr::pivot_wider(id_cols=pseudo_ID,names_from=variable,values_from=value)

tmp_sport <-
   r"{03_process_editions\%s\input\%s_hercodering_sport.xlsx}" |>
   sprintf(the_surv_id,the_surv_id) |>
   readxl::read_xlsx() |>
   tidyr::pivot_longer(-pseudo_ID) |>
   dplyr::filter_out(is.na(value)) |>
   dplyr::summarize(.by=pseudo_ID,spt_othx=stringr::str_c(name,collapse='; '))

tmp_correct_varlabels <- c(
	"pseudo_ID"="Uniek identificatienummer respondent van RR", # instead of "pseudo_ID"
	"INGEVULD"="Responsdispositie", # instead of "INGEVULD"
	"QRcode"="Response via QR of URL", # instead of "Ben je op deze vragenlijst terecht gekomen via een QR-code of niet?"
	"STARTTIJD"="Datum van respons", # instead of "STARTTIJD"
	"METHODIEK"="Respons modus", # instead of "METHODIEK"
	"Timer1"="Timer Start vragenlijst", # instead of "Timer1"
	"V1"="Geslacht", # instead of "Wat is uw geslacht?"
	"V2_1"="Geboortejaar", # instead of "In welk jaar bent u geboren?"
	"V2_2"="Geboortemaand", # instead of "In welke maand bent u geboren?"
	"V3"="Professionele situatie", # instead of "In welke situatie bevindt u zich momenteel? "
	"V3a"="Professionele situatie anders", # instead of "Een andere situatie (omschrijf):"
	"V4"="Diploma", # instead of "Wat is het hoogste diploma dat u heeft behaald? "
	"V4a"="Diploma", # instead of "Een ander diploma (omschrijf):"
	"V5_1"="Huishouden met partner", # instead of "[Met een partner] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5_2"="Huishouden met kinderen", # instead of "[Met kinderen (ook in co-ouderschap, ook volwassen kinderen)] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5_3"="Huishouden met (schoon)ouders", # instead of "[Met ouder(s)] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5_4"="Huishouden met broers/zussen", # instead of "[Met broers en/of zussen] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5_5"="Huishouden met andere familie", # instead of "[Met andere familieleden] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5_6"="Huishouden met vriend(en)", # instead of "[Met vrienden en/of vriendinnen] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5_7"="Huishouden met andere personen", # instead of "[Met andere personen (omschrijf):] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5_8"="Huishouden alleen", # instead of "[Ik woon alleen ] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5_9"="Huishouden met WN/GA", # instead of "[Weet niet/geen antwoord] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5a"="Huishouden met andere", # instead of "Met andere personen (omschrijf):"
	"V6"="Woonomgeving", # instead of "Hoe omschrijft u uw woonomgeving?"
	"V6a"="Woonomgeving andere", # instead of "Anders, omschrijf:"
	"V7"="Subjectief inkomen", # instead of "In welke mate kan uw gezin rondkomen met het beschikbare inkomen?"
	"V8"="Gezondheid", # instead of "Hoe is uw gezondheidstoestand op dit moment in het algemeen?"
	"V9"="Ziekte/handicap", # instead of "Hebt u één of meerdere langdurige ziekte(n), aandoening(en) of handicap(s)?"
	"V10"="Belemmering ziekte/handicap", # instead of "Hoe vaak wordt u belemmerd in uw dagelijkse bezigheden door deze ziekte(n), aandoening(en) of handicap(s)?"
	"Timer2"="Timer na afmaken module socio-demografische variabelen", # instead of "Timer2"
	"V11_1"="Lid sportclub", # instead of "[Een sportclub of -vereniging] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_2"="Lid hobbyclub", # instead of "[Een hobbyclub of -vereniging] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_3"="Lid kunst-/toneel-/muziekvereniging", # instead of "[Een kunst-, toneel- of muziekvereniging] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_4"="Lid socioculturele vereniging", # instead of "[Een socio-culturele vereniging (gezinsvereniging, vrouwenbeweging, …)] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_5"="Lid vakbond of werkgeversorganisatie", # instead of "[Een vakbond of een organisatie van werkgevers of zelfstandigen] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_6"="Lid wijk-/buurtcomité", # instead of "[Een wijk- of buurtcomité] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_7"="Lid oudervereniging/schoolraad", # instead of "[Een schoolcomité, schoolraad of oudervereniging] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_8"="Lid jeugd- of studentenvereniging", # instead of "[Een jeugdbeweging, -club of -vereniging, studentenvereniging] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_9"="Lid vereniging voor ouderen of gepensioneerden", # instead of "[Een vereniging voor ouderen of gepensioneerden] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_10"="Lid sociale vereniging", # instead of "[Een sociale vereniging die personen met een handicap, bejaarden, kansarmen, zieken, vreemdelingen … helpt] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_11"="Lid milieu- of natuurvereniging", # instead of "[Een milieu- of natuurvereniging] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_12"="Lid politieke partij of vereniging", # instead of "[Een politieke partij of vereniging] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_13"="Lid andere vereniging", # instead of "[Een andere vereniging (omschrijf): ] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_14"="Lid van geen vereniging", # instead of "[Ik ben van geen enkele vereniging actief lid] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11_15"="Lid weet niet", # instead of "[Weet niet/geen antwoord] Van welke verenigingen bent u momenteel lid en neemt u minstens af en toe deel aan de activiteiten?  U mag meerdere antwoorden aanduiden."
	"V11a"="Lid andere vereniging", # instead of "Een andere vereniging (omschrijf):"
	"V12"="Bestuurslid vereniging", # instead of "Bent u momenteel bestuurslid van één van die verenigingen?"
	"Timer3"="Timer na afmaken module Actief lidmaatschap van verenigingen", # instead of "Timer3"
	"V13"="Vrijwilligerswerk", # instead of "Hoe vaak doet u aan vrijwilligerswerk?"
	"Timer4"="Timer na afmaken module Vrijwilligerswerk", # instead of "Timer4"
	"V14_1"="Bezoek muziekoptredens", # instead of "[een muziekoptreden, -concert of -festival bijgewoond] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_2"="Bezoek bioscoop", # instead of "[een film in de bioscoop gezien] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_3"="Bezoek opera", # instead of "[een operavoorstelling bijgewoond] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_4"="Bezoek  dans-/balletvoorstelling", # instead of "[een dans- of balletvoorstelling bijgewoond] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_5"="Bezoek  theater/toneel", # instead of "[een theater- of toneelvoorstelling bijgewoond] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_6"="Bezoek circus", # instead of "[een circusvoorstelling bijgewoond] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_7"="Bezoek musical", # instead of "[een musical bijgewoond] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_8"="Bezoek cabaret-/standupoptreden", # instead of "[een cabaretvoorstelling, stand-up comedy of revue bijgewoond] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_9"="Bezoek museum/tentoonstelling", # instead of "[een museum, tentoonstelling of galerij bezocht] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_10"="Bezoek monument/gebouw", # instead of "[een bezienswaardig monument of gebouw bezocht] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"V14_11"="Bezoek bibliotheek", # instead of "[een boek ontleend in de bibliotheek] Hoe vaak heeft u in de voorbije 12 maanden …?"
	"Timer5"="Timer na afmaken module Cultuurparticipatie", # instead of "Timer5"
	"V15"="Sport frequentie", # instead of "Hoe vaak doet u aan sport?  Sport mag u ruim opvatten, ook wandelen en fietsen."
	"V16_1"="Beoefening wandelen", # instead of "[Wandelen] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_2"="Beoefening fietsen", # instead of "[Fietsen] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_3"="Beoefening lopen", # instead of "[Lopen] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_4"="Beoefening fitness", # instead of "[Fitness] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_5"="Beoefening zwemmen", # instead of "[Zwemmen] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_6"="Beoefening voetbal", # instead of "[Voetbal] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_7"="Beoefening volleybal", # instead of "[Volleybal] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_8"="Beoefening basketbal", # instead of "[Basketbal] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_9"="Beoefening tennis", # instead of "[Tennis] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_10"="Beoefening padel", # instead of "[Padel] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_11"="Beoefening yoga", # instead of "[Yoga] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_12"="Beoefening gevechtssport", # instead of "[Gevechtssport] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_13"="Beoefening dans", # instead of "[Dans] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_14"="Beoefening turnen", # instead of "[Turnen] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_15"="Beoefening andere sport", # instead of "[Andere sport(en) (omschrijf): ] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16_16"="Beoefening sport weet niet", # instead of "[Weet niet/geen antwoord] Welke sporten beoefent u momenteel?  U mag meerdere antwoorden aanduiden."
	"V16a"="Beoefening andere sport", # instead of "Andere sport(en) (omschrijf):"
	"Timer6"="Timer na afmaken module Sportparticipatie", # instead of "Timer6"
	"V17"="Levenstevredenheid", # instead of "Hoe tevreden bent u over uw leven in het algemeen?  U kan antwoorden met een score van 0 (heel ontevreden) tot 10 (heel tevreden)."
	"Timer7"="Timer na afmaken module Levenstevredenheid", # instead of "Timer7"
	"V18_1"="Tevredenheid woning", # instead of "[de woning waarin u woont] Hoe tevreden bent u met…?"
	"V18_2"="Tevredenheid buurt", # instead of "[de buurt waarin u woont] Hoe tevreden bent u met…?"
	"V18_3"="Tevredenheid inkomen", # instead of "[uw inkomen] Hoe tevreden bent u met…?"
	"V18_4"="Tevredenheid werk", # instead of "[uw werk ] Hoe tevreden bent u met…?"
	"V18_5"="Tevredenheid levensstandaard", # instead of "[uw levensstandaard] Hoe tevreden bent u met…?"
	"V18_6"="Tevredenheid gezondheid", # instead of "[uw gezondheid] Hoe tevreden bent u met…?"
	"V18_7"="Tevredenheid vrije tijd", # instead of "[uw bezigheden in uw vrije tijd] Hoe tevreden bent u met…?"
	"V18_8"="Tevredenheid contact met huisgenoten", # instead of "[de sociale contacten met uw huisgenoten] Hoe tevreden bent u met…?"
	"V18_9"="Tevredenheid contact met familie", # instead of "[de sociale contacten met uw familieleden] Hoe tevreden bent u met…?"
	"V18_10"="Tevredenheid contact met vrienden", # instead of "[de sociale contacten met uw vrienden en kennissen] Hoe tevreden bent u met…?"
	"V19_1"="Positief zijn over toekomst", # instead of "[Ik ben altijd optimistisch over mijn toekomst] In welke mate bent u het eens met volgende stellingen?"
	"V19_2"="Zorgen hebben over de toekomst", # instead of "[Ik maak me vaak zorgen over mijn toekomst] In welke mate bent u het eens met volgende stellingen?"
	"Timer8"="Timer na afmaken module welzijnAlgemeen welbevinden", # instead of "Timer8"
	"V20"="Verdere opmerkingen"  # instead of "Indien u nog opmerkingen kwijt wil, dan kan u deze hieronder noteren."
   )

data_survey <-
   r'{..\02_sourcedata\%s\02_survey}' |>
   sprintf(the_surv_id) |>
   file.path('SV2 - Datafile - finaal.sav') |>
   haven::read_sav() |>
   dplyr::filter(pseudo_ID %in% data_sample$pseudo_ID) |>
   coalesce_join(tmp_recodes,dplyr::join_by(pseudo_ID)) |>
   dplyr::left_join(tmp_sport,dplyr::join_by(pseudo_ID)) |>
   SVsurvey_labelled_2_tibble(the_surv_id,tmp_correct_varlabels) |>
   dplyr::select(-STEEKPROEF,-RESPNUM,-SURVEYJAAR,-PRIVACYINFO,-V20GOPM_1,-Exportfile) |>
   dplyr::mutate(
      Timer1 = STARTTIJD + lubridate::seconds_to_period(Timer1),
      Timer2 = Timer1 + lubridate::seconds_to_period(Timer2),
      Timer3 = Timer2 + lubridate::seconds_to_period(Timer3),
      Timer4 = Timer3 + lubridate::seconds_to_period(Timer4),
      Timer5 = Timer4 + lubridate::seconds_to_period(Timer5),
      Timer6 = Timer5 + lubridate::seconds_to_period(Timer6),
      Timer7 = Timer6 + lubridate::seconds_to_period(Timer7),
      Timer8 = Timer7 + lubridate::seconds_to_period(Timer8),
      STARTTIJD=lubridate::as_date(STARTTIJD),
      V2_1 = dplyr::replace_values(V2_1,1665~1965),
      INGEVULD = dplyr::replace_values(INGEVULD,
         "ja (minstens 1 vraag)"~"Antwoord",
         "geen antwoord (niets ontvangen)"~"Onbereikbaar",
         "neen, blanco / geen medewerking"~"Weigering",
         "ja, maar te laat geretourneerd (niet ingescand)"~"Te laat",
         "2.36"~'Verkeerde respondent',
         ),
      METHODIEK = dplyr::replace_values(METHODIEK,
         "online"~"Online",
         "schriftelijke vragenlijst"~"Schriftelijk"
         ),
      QRcode = dplyr::replace_values(QRcode,
         "ja, via een QR-code"~"QR",
         "neen, niet via een QR-code"~"URL"
         ),
      dplyr::across(
         tidyselect::where(is.character),
         \(x) dplyr::replace_values(x,
            'dubbel antwoord'~'Ongeldig',
            'geen opgave'~NA,
            'Niet geselecteerd'~'Neen',
            'Nee'~'Neen',
            'Niet van toepassing'~'Ongeldig'
            )
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




