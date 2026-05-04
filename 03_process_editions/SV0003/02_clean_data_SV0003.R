rm(list=ls())
devtools::load_all("00_functions")

the_surv_id <- 'SV0003'



# Load sample data --------------------------------------------------------

data_sample <-
   r"{..\02_sourcedata\%s\01_sample}" |>
   sprintf(the_surv_id) |>
   file.path("SV3_steekproef.xlsx") |>
   readxl::read_xlsx() |>
   dplyr::select(-CD_ZIP_RES,-gemeente,-geslacht,-CNTRY_BTH,-CNTRY_NAT,-survey_num) |>
   dplyr::mutate(CD_SEX=as.numeric(CD_SEX))


# Load survey data --------------------------------------------------------

tmp_recodes <-
   r"{03_process_editions\%s\input\%s_hercodering_obv_tekstvakken.xlsx}" |>
   sprintf(the_surv_id,the_surv_id) |>
   readxl::read_xlsx() |>
   tidyr::pivot_wider(id_cols=pseudo_ID,names_from=variable,values_from=value)

tmp_correct_varlabels <- c(
	"pseudo_ID"="Uniek identificatienummer respondent van RR", # instead of "pseudo_ID"
	"METHODIEK"="Respons modus", # instead of "METHODIEK"
	"QRcode"="Response via QR of URL", # instead of "Ben je op deze vragenlijst terecht gekomen via een QR-code of niet?"
	"STARTTIJD"="Datum van respons", # instead of "STARTTIJD"
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
	"V5_99"="Huishouden met WN/GA", # instead of "[Weet niet/geen antwoord] Met welke mensen leeft u samen in uw woning?    U mag meerdere antwoorden aanduiden."
	"V5a"="Huishouden met andere", # instead of "Met andere personen (omschrijf):"
	"V6"="Woonomgeving", # instead of "Hoe omschrijft u uw woonomgeving?"
	"V6a"="Woonomgeving andere", # instead of "Anders, omschrijf:"
	"V7"="Subjectief inkomen", # instead of "In welke mate kan uw gezin rondkomen met het beschikbare inkomen?"
	"V8"="Gezondheid", # instead of "Hoe is uw gezondheidstoestand op dit moment in het algemeen?"
	"V9"="Ziekte/handicap", # instead of "Hebt u één of meerdere langdurige ziekte(n), aandoening(en) of handicap(s)?"
	"V10"="Belemmering ziekte/handicap", # instead of "Hoe vaak wordt u belemmerd in uw dagelijkse bezigheden door deze ziekte(n), aandoening(en) of handicap(s)?"
	"Timer2"="Timer na afmaken module socio-demografische variabelen", # instead of "Timer2"
	"V11_1"="Bezit computer/laptop", # instead of "[Een computer of laptop] Welke van volgende toestellen heeft u thuis ter beschikking?  U mag meerdere antwoorden aanduiden."
	"V11_2"="Bezit tablet", # instead of "[Een tablet] Welke van volgende toestellen heeft u thuis ter beschikking?  U mag meerdere antwoorden aanduiden."
	"V11_3"="Bezit smartphone", # instead of "[Een smartphone] Welke van volgende toestellen heeft u thuis ter beschikking?  U mag meerdere antwoorden aanduiden."
	"V11_4"="Bezit gsm", # instead of "[Een andere mobiele telefoon of gsm] Welke van volgende toestellen heeft u thuis ter beschikking?  U mag meerdere antwoorden aanduiden."
	"V11_5"="Bezit smart tv", # instead of "[Een smart tv] Welke van volgende toestellen heeft u thuis ter beschikking?  U mag meerdere antwoorden aanduiden."
	"V11_6"="Bezit andere tv", # instead of "[Een ander televisietoestel] Welke van volgende toestellen heeft u thuis ter beschikking?  U mag meerdere antwoorden aanduiden."
	"V11_7"="Bezit radio", # instead of "[Een radiotoestel ] Welke van volgende toestellen heeft u thuis ter beschikking?  U mag meerdere antwoorden aanduiden."
	"V11_8"="Bezit geen toestellen", # instead of "[Geen van bovenstaande toestellen] Welke van volgende toestellen heeft u thuis ter beschikking?  U mag meerdere antwoorden aanduiden."
	"V11_99"="Bezit weet niet", # instead of "[Weet niet/geen antwoord] Welke van volgende toestellen heeft u thuis ter beschikking?  U mag meerdere antwoorden aanduiden."
	"V12"="Frequentie gebruik computer/laptop/tablet", # instead of "Hoe vaak gebruikt u een computer, laptop of tablet?"
	"V13"="Frequentie gebruik smartphone/gsm", # instead of "Hoe vaak gebruikt u een smartphone of andere mobiele telefoon (gsm)?"
	"Timer3"="Timer na afmaken module Mediabezit", # instead of "Timer3"
	"V14"="Toegang internet thuis", # instead of "Heeft u of iemand van uw gezin thuis toegang tot internet (via eender welk toestel)?"
	"V15_1"="Toegang vaste internetverbinding", # instead of "[Een vaste internetverbinding (eventueel met wifi)] Over welk soort internetverbinding(en) gaat het?  U mag meerdere antwoorden aanduiden."
	"V15_2"="Toegang mobiele internetverbinding", # instead of "[Een mobiele internetverbinding (3G, 4G, 5G), ook te gebruiken buitenshuis ] Over welk soort internetverbinding(en) gaat het?  U mag meerdere antwoorden aanduiden."
	"V15_3"="Toegang internetverbinding weet niet", # instead of "[Weet niet/geen antwoord] Over welk soort internetverbinding(en) gaat het?  U mag meerdere antwoorden aanduiden."
	"V16"="Frequentie internetgebruik", # instead of "Hoe vaak gebruikte u tijdens de afgelopen 3 maanden het internet, thuis of elders (via eender welk toestel)?"
	"V17_1"="Gebruik internet om te communiceren", # instead of "[Om te communiceren met anderen] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_2"="Gebruik internet voor telewerk", # instead of "[Om te telewerken] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_3"="Gebruik internet om info te zoeken", # instead of "[Om informatie op te zoeken] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_4"="Gebruik internet voor contact met overheid", # instead of "[Om met de overheid in contact te komen] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_5"="Gebruik internetbankieren", # instead of "[Om financiële zaken te regelen (internetbankieren)] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_6"="Gebruik internet voor online aankopen", # instead of "[Om online aankopen te doen] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_7"="Gebruik internet voor online verkopen", # instead of "[Om online zelf iets te verkopen] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_8"="Gebruik internet om te gamen", # instead of "[Om online spelletjes/games te spelen] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_9"="Gebruik internet voor muziek/tv", # instead of "[Om te kijken of te luisteren naar muziek, radio, tv of filmpjes] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
   "V17_10"="Gebruik internet om bij te leren", # instead of "[Om iets bij te leren via online cursussen of tutorials] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_11"="Gebruik internet om nieuws te volgen", # instead of "[Om het nieuws of de actualiteit te volgen] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_12"="Gebruik internet voor  sociale media", # instead of "[Om actief te zijn op sociale media] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_13"="Gebruik internet voor eigen media", # instead of "[Om eigen gemaakte foto’s, filmpjes, teksten, websites, … online te zetten] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_14"="Gebruik internet om routes te plannen", # instead of "[Om een route (reisweg) te plannen of te volgen ] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"V17_99"="Gebruik internet weet niet", # instead of "[Weet niet/geen antwoord] Voor welke van volgende activiteiten gebruikte u tijdens de afgelopen 3 maanden het internet?  U mag meerdere antwoorden aanduiden."
	"Timer4"="Timer na afmaken module Mediagebruik", # instead of "Timer4"
	"V18_1"="Toegang digitale tv", # instead of "[Digitale televisie via Telenet, Proximus, …] Heeft u of iemand van uw gezin toegang tot volgende betalende diensten?    U mag meerdere antwoorden aanduiden."
	"V18_2"="Toegang streaming films/series", # instead of "[Een streamingdienst voor films of series, bv. Netflix, Streamz, Disney+…] Heeft u of iemand van uw gezin toegang tot volgende betalende diensten?    U mag meerdere antwoorden aanduiden."
	"V18_3"="Toegang streaming muziek", # instead of "[Een streamingdienst voor muziek, bv. Spotify, iTunes …] Heeft u of iemand van uw gezin toegang tot volgende betalende diensten?    U mag meerdere antwoorden aanduiden."
	"V18_4"="Toegang abonnement krant", # instead of "[Een abonnement op een krant (online of op papier)] Heeft u of iemand van uw gezin toegang tot volgende betalende diensten?    U mag meerdere antwoorden aanduiden."
	"V18_5"="Toegang abonnement magazine", # instead of "[Een abonnement op een magazine (online of op papier) ] Heeft u of iemand van uw gezin toegang tot volgende betalende diensten?    U mag meerdere antwoorden aanduiden."
	"V18_6"="Geen toegang tot mediakanalen", # instead of "[Geen van deze diensten] Heeft u of iemand van uw gezin toegang tot volgende betalende diensten?    U mag meerdere antwoorden aanduiden."
	"V18_99"="Toegang betalende diensten Weet niet", # instead of "[Weet niet/geen antwoord] Heeft u of iemand van uw gezin toegang tot volgende betalende diensten?    U mag meerdere antwoorden aanduiden."
	"V19"="Frequentie tv kijken", # instead of "Hoe vaak kijkt u live of uitgesteld naar programma’s van televisiezenders (via eender welk toestel)?"
	"V20"="Frequentie radio luisteren", # instead of "Hoe vaak luistert u live of uitgesteld naar radioprogramma’s (via eender welk toestel)?"
	"V21"="Frequentie series streamen", # instead of "Hoe vaak kijkt u naar series of films van betalende streamingdiensten?"
	"V22"="Frequentie muziek streamen", # instead of "Hoe vaak luistert u naar muziek van betalende streamingdiensten?"
	"V23"="Frequentie krant lezen", # instead of "Hoe vaak leest u de krant (online of op papier)?"
	"V24"="Frequentie magazines lezen", # instead of "Hoe vaak leest u een magazine (online of op papier)?"
	"Timer5"="Timer na afmaken module Internetbezit", # instead of "Timer5"
	"V25"="Frequentie nieuws kijken via tv", # instead of "Hoe vaak kijkt u naar het nieuws van televisiezenders (via eender welk toestel)?"
	"V26"="Frequentie nieuws luisteren langs radio", # instead of "Hoe vaak luistert u naar het nieuws van radiozenders (via eender welk toestel)?"
	"V27"="Frequentie nieuws bekijken via het web", # instead of "Hoe vaak volgt u het nieuws via nieuwswebsites op internet?"
	"V28"="Frequentie nieuws volgen via sociale media", # instead of "Hoe vaak volgt u het nieuws via nieuwsberichten op sociale media?"
	"V29_1"="Vertrouwen in nieuws op tv", # instead of "[nieuwsprogramma’s van televisiezenders] Hoeveel vertrouwen heeft u in de juistheid van de berichtgeving van …?"
	"V29_2"="Vertrouwen in nieuws op de radio", # instead of "[nieuwsprogramma’s van radiozenders] Hoeveel vertrouwen heeft u in de juistheid van de berichtgeving van …?"
	"V29_3"="Vertrouwen in nieuws in de krant", # instead of "[kranten (online of op papier)] Hoeveel vertrouwen heeft u in de juistheid van de berichtgeving van …?"
	"V29_4"="Vertrouwen in nieuws op het web", # instead of "[nieuwswebsites op het internet] Hoeveel vertrouwen heeft u in de juistheid van de berichtgeving van …?"
	"V29_5"="Vertrouwen in nieuws op sociale media", # instead of "[nieuwsberichten op sociale media] Hoeveel vertrouwen heeft u in de juistheid van de berichtgeving van …?"
	"Timer6"="Timer na afmaken module Nieuwsgebruik", # instead of "Timer6"
	"V30_1"="Politiek actief door petities te tekenen", # instead of "[Een petitie getekend] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_2"="Politiek actief via sociale media", # instead of "[Uw politieke mening geuit door een bericht te posten of te delen op sociale media] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_3"="Politiek actief door betogingen", # instead of "[Deelgenomen aan een demonstratie of betoging] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_4"="Politiek actief via bijeenkomsten", # instead of "[Deelgenomen aan een politieke vergadering of bijeenkomst] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_5"="Politiek actief door overheidsinfo te verzamelen", # instead of "[Actief informatie verzameld over plannen of beslissingen van de overheid] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_6"="Politiek actief door contact met politici", # instead of "[Een politicus gecontacteerd om uw mening te uiten] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_7"="Politiek actief door contact met de pers", # instead of "[De pers gecontacteerd om uw mening te uiten] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_8"="Politiek actief via een burgerbeweging", # instead of "[Actief geweest in een burgerbeweging of actiecomité] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_9"="Politiek actief via een advies-/overlegorgaan", # instead of "[Deelgenomen aan een advies-, overleg- of inspraakorgaan van uw gemeente of stad] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_10"="Politiek actief via een politieke partij", # instead of "[Actief geweest binnen een politieke partij of vereniging] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_11"="Politiek actief op andere manier", # instead of "[Ik heb een andere politieke ac...
	"V30_12"="Geen politieke activiteiten uitgevoerd", # instead of "[Ik heb geen politieke activiteit gedaan] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30_99"="Politiek actief weet niet", # instead of "[Weet niet/geen antwoord] Welke van volgende activiteiten heeft u tijdens de afgelopen 12 maanden minstens 1 keer gedaan?  U mag meerdere antwoorden aanduiden."
	"V30a"="Politiek actief op andere manier", # instead of "Ik heb een andere politieke activiteit gedaan (omschrijf):"
	"Timer7"="Timer na afmaken module Politieke participatie", # instead of "Timer7"
	"V31_1"="Vertrouwen in de politie", # instead of "[de politie] Hoeveel vertrouwen heeft u in …?"
	"V31_2"="Vertrouwen in het onderwijs", # instead of "[het onderwijs] Hoeveel vertrouwen heeft u in …?"
	"V31_3"="Vertrouwen in het gerecht", # instead of "[het gerecht] Hoeveel vertrouwen heeft u in …?"
	"V31_4"="Vertrouwen in het leger", # instead of "[het leger] Hoeveel vertrouwen heeft u in …?"
	"V31_5"="Vertrouwen in politieke partijen", # instead of "[de politieke partijen] Hoeveel vertrouwen heeft u in …?"
	"V31_6"="Vertrouwen in werkgevers", # instead of "[de werkgevers] Hoeveel vertrouwen heeft u in …?"
	"V31_7"="Vertrouwen in vakbonden", # instead of "[de vakbonden] Hoeveel vertrouwen heeft u in …?"
	"V31_8"="Vertrouwen in de pers", # instead of "[de pers] Hoeveel vertrouwen heeft u in …?"
	"V32_1"="Vertrouwen in de gemeenteraad", # instead of "[de gemeenteraad] Hoeveel vertrouwen heeft u in …?"
	"V32_2"="Vertrouwen in de burgemeester en schepenen", # instead of "[het college van burgemeester en schepenen] Hoeveel vertrouwen heeft u in …?"
	"V32_3"="Vertrouwen in de gemeentelijke administratie", # instead of "[de gemeentelijke administratie] Hoeveel vertrouwen heeft u in …?"
	"V32_4"="Vertrouwen in de provincieraad", # instead of "[de provincieraad] Hoeveel vertrouwen heeft u in …?"
	"V32_5"="Vertrouwen in de provinciale gouverneur", # instead of "[de provinciegouverneur en de deputatie van de provincie] Hoeveel vertrouwen heeft u in …?"
	"V32_6"="Vertrouwen in de provinciale administratie", # instead of "[de provinciale administratie] Hoeveel vertrouwen heeft u in …?"
	"V32_7"="Vertrouwen in het Vlaams parlement", # instead of "[het Vlaams parlement] Hoeveel vertrouwen heeft u in …?"
	"V32_8"="Vertrouwen in de Vlaams regering", # instead of "[de Vlaamse regering] Hoeveel vertrouwen heeft u in …?"
	"V32_9"="Vertrouwen in de Vlaamse administratie", # instead of "[de Vlaamse administratie] Hoeveel vertrouwen heeft u in …?"
	"V32_10"="Vertrouwen in het federale parlement", # instead of "[het Belgisch of federale parlement] Hoeveel vertrouwen heeft u in …?"
	"V32_11"="Vertrouwen in de federale regering", # instead of "[de Belgische of federale regering] Hoeveel vertrouwen heeft u in …?"
	"V32_12"="Vertrouwen in de federale administratie", # instead of "[de Belgische of federale administratie] Hoeveel vertrouwen heeft u in …?"
	"V32_13"="Vertrouwen in de Europese commissie", # instead of "[de Europese commissie] Hoeveel vertrouwen heeft u in …?"
	"V32_14"="Vertrouwen in het Europees parlement", # instead of "[het Europees parlement] Hoeveel vertrouwen heeft u in …?"
	"Timer8"="Timer na afmaken module Vertrouwen in instellingen", # instead of "Timer8"
	"V33_1"="Tevredenheid over sportvoorzieningen", # instead of "[Sportvoorzieningen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_2"="Tevredenheid over culturele voorzieningen", # instead of "[Culturele voorzieningen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_3"="Tevredenheid over openbaar groen", # instead of "[Openbaar groen en natuurvoorzieningen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_4"="Tevredenheid over onderwijsinstellingen", # instead of "[Scholen en onderwijsinstellingen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_5"="Tevredenheid over gezondheidsvoorzieningen", # instead of "[Gezondheidsvoorzieningen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_6"="Tevredenheid over huisvuilvoorzieningen", # instead of "[Huisvuilvoorzieningen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_7"="Tevredenheid over het openbaar vervoer", # instead of "[Het openbaar vervoer met tram en bus] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_8"="Tevredenheid over fiets- en voetpaden", # instead of "[De staat van fiets- en voetpaden] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_9"="Tevredenheid over de wegen", # instead of "[De staat van de wegen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_10"="Tevredenheid over de begeleiding van werklozen", # instead of "[Voorzieningen voor de begeleiding van werklozen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_11"="Tevredenheid over de opvang van vreemdelingen", # instead of "[Voorzieningen voor de opvang en begeleiding van vreemdelingen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_12"="Tevredenheid over de opvang van kansarmen", # instead of "[Voorzieningen voor de opvang en begeleiding van kansarmen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_13"="Tevredenheid over voorzieningen voor personene met een handicap", # instead of "[Voorzieningen voor personen met een handicap] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_14"="Tevredenheid over speel- en jongerenvoorzieningen", # instead of "[Speel- en jongerenvoorzieningen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_15"="Tevredenheid over voorzieningen voor ouderen", # instead of "[Ouderenvoorzieningen] Hoe tevreden bent u over volgende voorzieningen?"
	"V33_16"="Tevredenheid over kinderopvang", # instead of "[Voorzieningen voor kinderopvang] Hoe tevreden bent u over volgende voorzieningen?"
	"V34"="Verdere opmerkingen"  # instead of "Indien u nog opmerkingen kwijt wil, dan kan u deze hieronder noteren."
	)

data_survey <-
   r'{..\02_sourcedata\%s\02_survey}' |>
   sprintf(the_surv_id) |>
   file.path('SV3 - Datafile - finaal.sav') |>
   haven::read_sav() |>
   dplyr::filter(pseudo_ID %in% data_sample$pseudo_ID) |>
   coalesce_join(tmp_recodes,dplyr::join_by(pseudo_ID)) |>
   SVsurvey_labelled_2_tibble(the_surv_id,tmp_correct_varlabels) |>
   dplyr::select(-STEEKPROEF,-RESPNUM,-SURVEYJAAR,-PRIVACYINFO,-V34GOPM_1,-Exportfie) |>
   dplyr::mutate(
      Timer1 = STARTTIJD + lubridate::seconds_to_period(Timer1),
      Timer2 = Timer1 + lubridate::seconds_to_period(Timer2),
      Timer3 = Timer2 + lubridate::seconds_to_period(Timer3),
      Timer4 = Timer3 + lubridate::seconds_to_period(Timer4),
      Timer5 = Timer4 + lubridate::seconds_to_period(Timer5),
      Timer6 = Timer5 + lubridate::seconds_to_period(Timer6),
      Timer7 = Timer6 + lubridate::seconds_to_period(Timer7),
      Timer8 = Timer7 + lubridate::seconds_to_period(Timer8),
      Timer9 = Timer8 + lubridate::seconds_to_period(Timer9),
      STARTTIJD=lubridate::as_date(STARTTIJD),
      INGEVULD = dplyr::replace_values(INGEVULD,
         "ja (minstens 1 vraag)"~"Antwoord",
         "geen antwoord (niets ontvangen)"~"Onbereikbaar",
         "neen, blanco / geen medewerking"~"Weigering",
         "ja, maar te laat geretourneerd (niet ingescand)"~"Te laat",
         "2.36"~'Verkeerde respondent',
         "2.32"~"Niet bekwaam",
         ),
      METHODIEK = dplyr::replace_values(METHODIEK,
         "online"~"Online",
         "schriftelijke vragenlijst"~"Schriftelijk"
         ),
      QRcode = dplyr::replace_values(QRcode,
         "Ja, via een QR-code"~"QR",
         "Neen, niet via een QR-code"~"URL"
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
      V1 = dplyr::replace_values(V1,"anders"~"X"),
      V6 = dplyr::replace_values(V6,"Het centrum van een (deel) gemeente"~"Het centrum van een (deel)gemeente"),
      V8 = dplyr::replace_values(V8,"99"~"Ongeldig")
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




