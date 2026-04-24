#' #' Create categorical age
#' #'
#' #' Recode a continuous age vector into a categorical age vector with 5-years age categories.
#' #'
#' #' @param age A numeric vector with ages.
#' #'
#' #' @returns A factor with levels equal to 5-years age categories.
#' #' @export
#' #'
#' #' @examples
#' #' age =-10:120
#' #' agecat = age2agecat5yr(age)
#' age2agecat5yr <- function(age){
#'   cut(age,
#'     breaks=c(seq(0,100,5),Inf),
#'     labels=c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-99','100+'),
#'     right =FALSE
#'     )
#'   }
#'
#'
#'
#'
#' #' Create categorical age
#' #'
#' #' Recode a continuous age vector into a categorical age vector with categories 0-17 year/18-66 year/67 year and older.
#' #'
#' #' @param age A numeric vector with ages.
#' #'
#' #' @returns A factor with levels equal to 0-17 year / 18-66 year / 67 year and older.
#' #' @export
#' #'
#' #' @examples
#' #' age =-10:120
#' #' agecat = age2agecat3a(age)
#' age2agecat3a <- function(age){
#'   cut(age,
#'     breaks=c(0,18,67,Inf),
#'     labels=c('0-17','18-66','67+'),
#'     right =FALSE
#'     )
#'   }
#'
#'
#'
#' #' Create categorical age
#' #'
#' #' Recode a continuous age vector into a categorical age vector with categories 0-19 year/20-64 year/65 year and older.
#' #'
#' #' @param age A numeric vector with ages.
#' #'
#' #' @returns A factor with levels equal to 0-19 year/20-64 year/65 year and older.
#' #' @export
#' #'
#' #' @examples
#' #' age =-10:120
#' #' agecat = age2agecat3b(age)
#' age2agecat3b <- function(age){
#'   cut(age,
#'     breaks=c(0,20,65,Inf),
#'     labels=c('0-19','20-64','65+'),
#'     right =FALSE
#'     )
#' }
#'
#'
#'
#'
#' #' Create categorical age
#' #'
#' #' Recode a continuous age vector into a categorical age vector with categories 0-17 year / 18-39 year / 40-66 year / 67 year and older.
#' #'
#' #' @param age A numeric vector with ages.
#' #'
#' #' @returns A factor with levels equal to 0-17 year / 18-39 year / 40-66 year / 67 year and older.
#' #' @export
#' #'
#' #' @examples
#' #' age =-10:120
#' #' agecat = age2agecat4a(age)
#' age2agecat4a <- function(age){
#'   cut(age,
#'     breaks=c(0,18,40,67,Inf),
#'     labels=c('0-17','18-39','40-66','67+'),
#'     right =FALSE
#'     )
#'   }
#'
#'
#'
#'
#' #' Create categorical age
#' #'
#' #' Recode a continuous age vector into a categorical age vector per 5 years, but only from 18 years.
#' #'
#' #' @param age A numeric vector with ages.
#' #'
#' #' @returns A factor with levels equal to 5-years age categories, from 18 years.
#' #' @export
#' #'
#' #' @examples
#' #' age =-10:120
#' #' agecat = age2agecat5yrfrom18(age)
#' age2agecat5yrfrom18 <- function(age){
#'   cut(age,
#'     breaks=c(18,seq(25,90,5),Inf),
#'     labels=c('18-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85-89','90+'),
#'     right =FALSE
#'     )
#'   }



#' Create categorical age
#'
#' Recode a continuous age vector into a categorical age vector per 10 years, but only from 18 years.
#'
#' @param age A numeric vector with ages.
#'
#' @returns A factor with levels equal to 10-years age categories, from 18 years.
#' @export
#'
#' @examples
#' age =-10:120
#' agecat = age2agecat7from18(age)
age2agecat7from18 <- function(age){
  cut(age,
    breaks=c(18,seq(25,75,10),Inf),
    labels=c('a18_24','a25_34','a35_44','a45_54','a55_64','a65_74','a75plus'),
    right =FALSE
    )
  }



#' Create categorical age
#'
#' Recode a continuous age vector into a categorical age per +-20 years, but only from 18 years.
#'
#' @param age A numeric vector with ages.
#'
#' @returns A factor with levels equal to 20-years age categories, from 18 years.
#' @export
#'
#' @examples
#' age =-10:120
#' agecat = age2agecat4from18(age)
age2agecat4from18 <- function(age){
  cut(age,
    breaks=c(18,35,50,65,Inf),
    labels=c('a18_34','a35_49','a50_64','a65plus'),
    right =FALSE
    )
  }



#' #' Create categorical age
#' #'
#' #' Recode a continuous age vector into a categorical age vector per 10 years, but only from 18 years.
#' #'
#' #' @param age A numeric vector with ages.
#' #'
#' #' @returns A factor with levels equal to 10-years age categories, from 18 years.
#' #' @export
#' #'
#' #' @examples
#' #' age =-10:120
#' #' agecat = age2agecat9from18(age)
#' age2agecat9from18 <- function(age){
#'   cut(age,
#'     breaks=c(18,25,seq(30,90,10),Inf),
#'     labels=c('18-24','25-29','30-39','40-49','50-59','60-69','70-79','80-89','90+'),
#'     right =FALSE
#'     )
#'   }

