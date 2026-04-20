#' Check if a String is a Valid Regular Expression
#'
#' Attempts to evaluate a regular expression pattern against an empty string
#' to determine whether the pattern is syntactically valid.
#'
#' @param pattern A character string containing the regular expression pattern
#'   to validate.
#'
#' @return A logical value: \code{TRUE} if the pattern is a valid regular
#'   expression, \code{FALSE} otherwise.
#'
#' @examples
#' is_valid_regex("[a-z]+")  # TRUE
#' is_valid_regex("[a-z")    # FALSE (invalid regex)
#'
#' @export
is_valid_regex <- function(pattern) {
  answer <- FALSE
  try({suppressWarnings(grepl(pattern, "", perl = TRUE)); answer <- TRUE}, silent = TRUE)
  return(answer)
}