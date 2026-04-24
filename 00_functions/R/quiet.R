#' Suppress All Output From an Expression
#'
#' Evaluates an expression while suppressing all printed output and messages
#' sent to \code{stdout}. Unlike \code{\link{suppressMessages}} or
#' \code{\link{suppressWarnings}}, this function captures \emph{all} console
#' output by redirecting \code{stdout} to a temporary file.
#'
#' @param x An expression to evaluate quietly.
#'
#' @return The result of \code{x}, returned invisibly.
#'
#' @details
#' Output is suppressed by redirecting \code{stdout} to a temporary file via
#' \code{\link{sink}}. The sink is guaranteed to be released on exit via
#' \code{\link{on.exit}}, even if \code{x} throws an error.
#'
#' Note that this function suppresses printed output only. Warnings and
#' messages sent via \code{\link{warning}} or \code{\link{message}} are
#' \emph{not} suppressed. To suppress those as well, wrap the call in
#' \code{\link{suppressWarnings}} and/or \code{\link{suppressMessages}}:
#'
#' \preformatted{
#' suppressWarnings(suppressMessages(quiet(x)))
#' }
#'
#' @examples
#' # Printed output is suppressed
#' quiet(print("this will not appear"))
#'
#' # Return value is still accessible
#' result <- quiet(1 + 1)
#'
#' @export
quiet <- function(x) {
   sink(tempfile())
   on.exit(sink())
   invisible(force(x))
   }