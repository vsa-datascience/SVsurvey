validate_data <- function(data,dsd,order_variables=TRUE,silent=FALSE) {

   # VALIDATION PARAMS -------------------------------------------------------

   if ( !tibble::is_tibble(data) ) {
      stop('Object dsd is not a tibble.')
      }
   if ( !is.data.frame(data) ){
      stop('Object data is not a data.frame.')
      }


   # Temporarily widen output options for readable error messages ------------
   old <- options(warning.length = 8170, width = 10000)
   on.exit(options(old), add = TRUE)


   # CREATE EMPTY ERROR VECTOR -----------------------------------------------

   errors = NULL


   # VALIDATION VARIABLES ----------------------------------------------------

   # check all data variables in data model
   tmp <- setdiff(names(data),dsd$concept_id) |>
      sprintf(fmt='Variable in dataset but not in dsd: %s')
   errors <- c(errors,tmp)

   # check all data model variables in data
   tmp <- setdiff(dsd$concept_id,names(data)) |>
      sprintf(fmt='Variable in dsd but not in dataset: %s')
   errors <- c(errors,tmp)

   # update dsd
   dsd <- dsd |>
      dplyr::filter(concept_id %in% names(data))



   # VALIDATION VARIABLE CLASSES ---------------------------------------------

   # check class of variables
   tmp_classes <- list(
      "character"=c('character'),
      "date"=c('Date','character'),
      "datetime"=c("POSIXct","POSIXt",'character'),
      "codelist"=c('character','factor','numeric'),
      "integer"=c('numeric','integer'),
      "numeric"=c('numeric','integer')
      )

   dsd <- dsd |>
      dplyr::mutate(
         class=purrr::map(concept_id,\(x) class(data[[x]])),
         test=purrr::map2_lgl(class,constraint_type,\(x,y) any(x %in% tmp_classes[[y]])),
         )

   tmp <- dsd |>
      dplyr::filter(!test) |>
      dplyr::mutate(
         message=purrr::map_chr(constraint_type,\(x) tmp_classes[[x]] |> stringr::str_flatten_comma(last=' or ')),
         message=sprintf(
            '%s has wrong class: %s instead of %s.',
            concept_id,class,message
            )
         ) |>
      dplyr::pull(message)

   errors <- c(errors,tmp)

   # update dsd
   dsd <- dsd |>
      dplyr::filter(test) |>
      dplyr::select(-test)



   # VALIDATION VALUES ----------------------------------------------------

   # check allow_na
   tmp <- dsd |>
      dplyr::filter(constraint_allow_missings==FALSE) |>
      dplyr::mutate(test = purrr::map_lgl(concept_id,\(x) data[[x]] |> is.na() |> any() )) |>
      dplyr::filter(test) |>
      dplyr::mutate(message=sprintf('%s has missing values, but that\'s not allowed.',concept_id)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   # check regex
   tmp <- dsd |>
      dplyr::filter_out(is.na(constraint_regex)) |>
      dplyr::mutate(message = purrr::map2(concept_id,constraint_regex,\(x,y) stringr::str_subset(data[[x]],y,negate=TRUE) |> paste(collapse='; '))) |>
      dplyr::filter(message!='') |>
      dplyr::mutate(message=sprintf('%s: values do not satisfy regular expression "%s": %s',concept_id,constraint_regex,message)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   # check character dates
   tmp_regex <- '^\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])$'
   tmp <- dsd |>
      dplyr::filter(constraint_type=='date' & class=='character') |>
      dplyr::mutate(test = purrr::map_lgl(concept_id,\(x) data[[x]] |> stringr::str_detect(tmp_regex,negate=TRUE) |> any())) |>
      dplyr::filter(test) |>
      dplyr::mutate(message=sprintf('%s has wrong format: It is character date but its values must be "YYYY-MM-DD".',concept_id)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   # check character datetimes
   tmp_regex <- '^\\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])[ T](0[0-9]|1[0-9]|2[0-3]):[0-5]\\d:[0-5]\\d$'
   tmp <- dsd |>
      dplyr::filter(constraint_type=='datetime' & class=='character') |>
      dplyr::mutate(test = purrr::map_lgl(concept_id,\(x) data[[x]] |> stringr::str_detect(tmp_regex,negate=TRUE) |> any())) |>
      dplyr::filter(test) |>
      dplyr::mutate(message=sprintf('%s has wrong format: It is character date but its values must be "YYYY-MM-DD HH:MM:SS".',concept_id)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   # check min
   tmp <- dsd |>
      dplyr::filter_out(is.na(constraint_min)) |>
      dplyr::mutate(test=purrr::map2_lgl(concept_id,constraint_min,\(x,y) any(data[[x]]<y))) |>
      dplyr::filter(test) |>
      dplyr::mutate(message=sprintf('%s: values smaller than %s, which is not allowed.',concept_id,constraint_min)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   # check max
   tmp <- dsd |>
      dplyr::filter_out(is.na(constraint_max)) |>
      dplyr::mutate(test=purrr::map2_lgl(concept_id,constraint_max,\(x,y) any(data[[x]]>y))) |>
      dplyr::filter(test) |>
      dplyr::mutate(message=sprintf('%s: values larger than %s, which is not allowed.',concept_id,constraint_max)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   # check codelists
   codelists <- dsd |>
      dplyr::filter(constraint_type=='codelist' ) |>
      dplyr::mutate(
         hasvalue  = purrr::map_lgl(constraint_codelist_ds,\(x) 'value'  %in% names(x)),
         hasvaluen = purrr::map_lgl(constraint_codelist_ds,\(x) 'valuen' %in% names(x)),
         )

   tmp <- codelists |>
      dplyr::filter(class=='numeric' & !hasvaluen) |>
      dplyr::mutate(message=sprintf('%s: is numeric but has no valuen column in codelist',concept_id)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   tmp <- codelists |>
      dplyr::filter(class %in% c('character','factor') & !hasvalue) |>
      dplyr::mutate(message=sprintf('%s: is character/factor but has no value column in codelist',concept_id)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   tmp <- codelists |>
      dplyr::filter(class=='numeric' & hasvaluen) |>
      dplyr::mutate(
         constraint_codelist_ds=purrr::map(constraint_codelist_ds,\(x) dplyr::pull(x,'valuen')),
         message1=purrr::map2_chr(concept_id,constraint_codelist_ds,\(x,y) data[[x]] |> na.omit() |> setdiff(y) |> stringr::str_c(collapse=', ') ),
         message2=purrr::map(constraint_codelist_ds,\(x) stringr::str_flatten(paste0('"',x,'"'),collapse=", ",last=" or "))
         ) |>
      dplyr::filter(message1!='') |>
      dplyr::mutate(message=sprintf('%s: numeric values not in codelist: %s\n\tShould be from %s',concept_id,message1,message2)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   tmp <- codelists |>
      dplyr::filter(class %in% c('character','factor') & hasvalue) |>
      dplyr::mutate(
         constraint_codelist_ds=purrr::map(constraint_codelist_ds,\(x) dplyr::pull(x,'value')),
         message1=purrr::map2(concept_id,constraint_codelist_ds,\(x,y) data[[x]] |> na.omit() |> setdiff(y) |> sprintf(fmt='"%s"') |> stringr::str_c(collapse=', ') ),
         message2=purrr::map(constraint_codelist_ds,\(x) stringr::str_flatten(paste0('"',x,'"'),collapse=", ",last=" or "))
         ) |>
      dplyr::filter(message1!='') |>
      dplyr::mutate(message=sprintf('%s: character values not in codelist: %s\n\tShould be from %s',concept_id,message1,message2)) |>
      dplyr::pull(message)
   errors <- c(errors,tmp)

   # throw errors

   if (length(errors)>0) {
     errors <- paste(errors,collapse='\n')
     stop('\n',errors)
     }



   # Clean data --------------------------------------------------------------

   ### transform all codelist variables to factors
   tmp_num <- codelists |>
      dplyr::filter(class=='numeric') |>
      dplyr::pull(constraint_codelist_ds,concept_id)
   tmp_chr <- codelists |>
      dplyr::filter(class=='character') |>
      dplyr::pull(constraint_codelist_ds,concept_id)
   data <- dplyr::mutate(
      data,
      dplyr::across(
         tidyselect::all_of(names(tmp_num)),
         \(x) factor(x,levels=tmp_num[[dplyr::cur_column()]][['valuen']],labels=tmp_num[[dplyr::cur_column()]][['value']])),
      dplyr::across(
         tidyselect::all_of(names(tmp_chr)),
         \(x) factor(x,levels=tmp_chr[[dplyr::cur_column()]][['value']]))
      )
   rm(tmp_num,tmp_chr)

   ### transform all character date variables to dates
   tmp_date <- dsd |>
      dplyr::filter(constraint_type=='date' & class=='character') |>
      dplyr::pull(concept_id)
   data <- dplyr::mutate(
      data,
      dplyr::across(tidyselect::all_of(tmp_date),as.Date)
      )
   rm(tmp_date)

   ### transform all character datetime variables to datetimes
   tmp_date <- dsd |>
      dplyr::filter(constraint_type=='date' & class=='character') |>
      dplyr::pull(concept_id)
   data <- dplyr::mutate(
      data,
      dplyr::across(tidyselect::all_of(tmp_date),lubridate::ymd_hms)
      )
   rm(tmp_date)

   ### make numeric variables numeric
   tmp_int <- dplyr::filter(dsd,constraint_type=='integer') |> dplyr::pull(concept_id)
   tmp_num <- dplyr::filter(dsd,constraint_type=='double') |> dplyr::pull(concept_id)
   data <- dplyr::mutate(
      data,
      dplyr::across(tidyselect::all_of(tmp_int),as.integer),
      dplyr::across(tidyselect::all_of(tmp_num),as.numeric),
      )
   rm(tmp_int,tmp_num)

   ### put variables in the order of the dsd
   if ( order_variables ) {
      data <- dplyr::select(
         data,
         tidyselect::all_of(dsd$concept_id)
         )
      }

   ### message
   if (!silent) { cat("\033[32mData is cleaned!\033[0m\n") }

   # Output ------------------------------------------------------------------

   return(data)

}
