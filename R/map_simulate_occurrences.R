#' Map `simulate_occurrences()` function over multiple species
#'
#' The function executes `simulate_occurrences()` over multiple rows of a
#' dataframe, representing multiple different species, containing potentially
#' different function arguments over multiple columns.
#'
#' @param df ...
#' @param nested Logical. If `TRUE` (default), ... Otherwise ...
#' @param arg_list ...
#'
#' @returns ...
#'
#' @export
#'
#' @import dplyr
#' @import assertthat
#'
#' @family multispecies
#'
#' @examples
#'

map_simulate_occurrences <- function(
    df,
    nested = TRUE,
    arg_list = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if df is a dataframe
  stopifnot("`df` must be a dataframe." = inherits(df, "data.frame"))

  # Check if nested is a logical vector of length 1
  stopifnot("`nested` must be a logical vector of length 1." =
              assertthat::is.flag(nested) && assertthat::noNA(nested))

  ### End checks

  # Rename column names if necessary
  if (!is.na(arg_list)) {
    df <- df %>%
      dplyr::rename(!!!arg_list)
  }

  # Map function over all rows
  out_df <- map_simulation_functions(
    f = simulate_occurrences,
    df = df,
    nested = nested)

  # Rename nested output column
  if (nested) {
    out_df <- out_df %>%
      dplyr::rename("occurrences", "mapped_col")
  }

  return(out_df)
}
