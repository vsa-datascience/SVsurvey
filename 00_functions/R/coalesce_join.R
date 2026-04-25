#' Coalesce Join: Replace Values Using a Replacement Dataset
#'
#' Performs a left join and uses \code{dplyr::coalesce()} to replace values in
#' \code{originaldata} with non-\code{NA} values from \code{replacementdata}.
#' Only overlapping (non-key) columns are updated; columns exclusive to either
#' dataset are unaffected.
#'
#' @param originaldata A \code{data.frame} or \code{tibble}. The base dataset
#'   whose values will be (partially) replaced.
#' @param replacementdata A \code{data.frame} or \code{tibble} containing
#'   replacement values. All its columns must also exist in \code{originaldata}.
#' @param by A join specification in one of three formats:
#'   \itemize{
#'     \item \code{NULL} (default): all shared column names are used as keys.
#'     \item A named or unnamed character vector, following the same convention
#'       as \code{dplyr::left_join()} (e.g. \code{c("id_original" = "id_replacement")}).
#'     \item A \code{\link[dplyr]{join_by}()} expression. Only equi-joins
#'       (\code{==}) are supported.
#'   }
#'
#' @return A \code{tibble} with the same columns and row order as
#'   \code{originaldata}, where non-\code{NA} values from \code{replacementdata}
#'   have overwritten the corresponding values in shared columns.
#'
#' @details
#' The function enforces the following checks before joining:
#' \enumerate{
#'   \item \code{originaldata} and \code{replacementdata} must both be
#'     data frames or tibbles.
#'   \item All columns in \code{replacementdata} must exist in
#'     \code{originaldata} — adding new columns via this function is not
#'     supported.
#'   \item All rows in \code{replacementdata} must match at least one row in
#'     \code{originaldata} on the join keys — orphaned replacement rows are
#'     likely a data error and will raise an informative error message.
#'   \item If \code{by} is a \code{join_by()} expression, only equi-joins
#'     are permitted.
#' }
#' A warning is issued if \code{replacementdata} contains duplicate keys, as
#' this would result in a one-to-many join.
#'
#' Coalescing is applied column-by-column: for each shared non-key column,
#' the replacement value takes precedence over the original wherever it is
#' non-\code{NA}.
#'
#' @examples
#' \dontrun{
#' # Using a named character vector
#' corrections <- tibble(id = 2L, score = 99)
#' result <- coalesce_join(originaldata, corrections, by = c("id" = "id"))
#'
#' # Using join_by()
#' result <- coalesce_join(originaldata, corrections, by = join_by(id))
#' }
#'
#' @importFrom dplyr anti_join left_join coalesce select mutate summarize
#'   across everything pull n row_number all_of
#' @importFrom tidyr pivot_longer replace_na
#'
#' @export
coalesce_join <- function(originaldata, replacementdata, by = NULL) {

   # Input type checks --------------------------------------------------------
   if (!is.data.frame(originaldata)) stop("`originaldata` must be a data.frame or tibble.")
   if (!is.data.frame(replacementdata)) stop("`replacementdata` must be a data.frame or tibble.")

   # Derive key column names from `by` ----------------------------------------
   if (is.null(by)) {
      by_x <- intersect(names(originaldata), names(replacementdata))
      by_y <- by_x
   } else if (inherits(by, "dplyr_join_by")) {
      if (!all(by$condition == "=="))
         stop("`coalesce_join()` only supports equi-joins. ",
              "Please use only `==` conditions in `join_by()`.")
      by_x <- by$x
      by_y <- by$y
   } else if (is.null(names(by)) || all(names(by) == "")) {
      # Unnamed character vector: c("id") — same column name on both sides
      by_x <- by
      by_y <- by
   } else {
      # Named character vector: c(left_col = "right_col")
      by_x <- ifelse(names(by) == "", by, names(by))
      by_y <- unname(by)
   }

   # Check 1: no new columns in replacementdata --------------------------------
   missingvar <- names(replacementdata) |>
      setdiff(names(originaldata))
   if (length(missingvar) > 0) {
      missingvar <- paste(missingvar, collapse = ", ")
      stop("Columns present in `replacementdata` but not in `originaldata`: ",
           missingvar, ". `coalesce_join()` cannot add new columns.")
   }

   # Check 2: no orphaned rows in replacementdata ------------------------------
   missingcases <- replacementdata |> dplyr::anti_join(originaldata, by = by)
   if (nrow(missingcases) > 0) {
      missingcases <- missingcases |>
         dplyr::select(dplyr::all_of(by_y)) |>
         dplyr::mutate(
            dplyr::across(dplyr::everything(),
                          ~ .x |> as.character() |> replace_na("NA")),
            i = dplyr::row_number()
         ) |>
         tidyr::pivot_longer(-i) |>
         dplyr::mutate(value = paste0(name, "=", value)) |>
         dplyr::summarize(value = paste0(value, collapse = ", "), .by = i) |>
         dplyr::summarize(value = paste0(value, collapse = "; ")) |>
         dplyr::pull(value)
      stop("Rows in `replacementdata` with no match in `originaldata`: ",
           missingcases)
   }

   # Check 3: warn on duplicate keys in replacementdata -----------------------
   dup_keys <- replacementdata |>
      dplyr::select(dplyr::all_of(by_y)) |>
      dplyr::filter(dplyr::n() > 1, .by = dplyr::all_of(by_y))
   if (nrow(dup_keys) > 0)
      warning("`replacementdata` contains duplicate keys. ",
              "Only the first match will be used per row of `originaldata`.")

   # Coalesce join ------------------------------------------------------------
   coalvars <- setdiff(
      intersect(names(originaldata), names(replacementdata)),
      by_x
      )
   joined <- dplyr::left_join(originaldata, replacementdata,
                       by = by, suffix = c("", "_tocoalesce"))
   for (var in coalvars) {
      joined[[var]] <- dplyr::coalesce(joined[[paste0(var, "_tocoalesce")]],
                                       joined[[var]])
   }

   return(dplyr::select(joined, dplyr::all_of(names(originaldata))))
}