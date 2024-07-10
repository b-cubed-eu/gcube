#' Map `simulate_occurrences()` function over multiple species
#'
#' The function executes `simulate_occurrences()` over multiple rows of a
#' dataframe, representing multiple different species, containing potentially
#' different function arguments over multiple columns.
#'
#' @param df A dataframe containing multiple rows. Each row is considered a
#' different species. The columns are function arguments with values used for
#' mapping `simulate_occurrences()` for each species. `df` can have columns that
#' are not used by this function. They will be retained in the output.
#' @param nested Logical. If `TRUE` (default), retain list-column containing
#' sf objects calculated by `simulate_occurrences()`. Otherwise, expand this
#' list-column into rows and columns.
#' @param arg_list A named list or `NA`. If `NA` (default), the function assumes
#' column names in `df` are identical to argument names of
#' `simulate_occurrences()` and the function specified in its
#' `temporal_function` argument. If column names are not identical, they need to
#' be specified as a named list where the names are the argument names of
#' `simulate_occurrences()` or the function specified in its `temporal_function`
#' argument, and their associated values a string of the corresponding column
#' name in `df`. If `temporal_function` is a custom function containing the
#' ellipsis argument (`...`), you need to specify any arguments used for this
#' ellipsis in this list as well.
#'
#' @returns In case of `nested = TRUE`, a dataframe identical to the input
#' dataframe `df`, but with an extra list-column called `occurrences` containing
#' an sf object with POINT geometry for each row computed by
#' `simulate_occurrences()`. In case of `nested = FALSE`, this list-column is
#' expanded into additional rows and columns.
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

  # Check if arg_list is a named list

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
