#' Map cube simulation functions over multiple rows of a dataframe
#'
#' The function executes a cube simulation function (`simulate_occurrences()`,
#' `sample_observations()` or `grid_designation()`) over multiple rows of a
#' dataframe containing potentially different function arguments over multiple
#' columns.
#'
#' @param f One of three cube simulation functions: `simulate_occurrences()`,
#' `sample_observations()` or `grid_designation()`.
#' @param df A dataframe containing multiple rows. Each row is considered a
#' different species. The columns are function arguments with values used for
#' mapping `simulate_occurrences()` for each species. `df` can have columns that
#' are not used by this function. They will be retained in the output.
#' @param nested Logical. If `TRUE` (default), retain list-column containing
#' dataframes calculated by `simulate_occurrences()`. Otherwise, expand this
#' list-column into rows and columns.
#' @param extra_arg_names A character vector of extra arguments from the custom
#' temporal_function if it contains and ellipsis (`...`). `NULL` by default
#' (no extra arguments).
#'
#' @returns In case of `nested = TRUE`, a dataframe identical to the input
#' dataframe `df`, but with an extra list-column called `mapped_col` containing
#' an sf object for each row computed by the function specified in `f`. In case
#' of `nested = FALSE`, this list-column is expanded into additional rows and
#' columns.
#'
#' @import dplyr
#' @importFrom purrr pmap
#' @importFrom tidyr unnest
#'
#' @examples
#'

map_simulation_functions <- function(
    f,
    df,
    nested = TRUE,
    extra_arg_names = NULL) {
  ### Start checks
  # 1. Check input type and length
  # Check if f is a function
  function_message <- paste("`f` must be a function.\nOne of",
                            "`simulate_occurrences()`, `sample_observations()`",
                            "or `grid_designation()`.")
  do.call(stopifnot, setNames(list(inherits(f, "function")), function_message))

  # Check if df is a dataframe
  stopifnot("`df` must be a dataframe." = inherits(df, "data.frame"))

  # Check if nested is a logical vector of length 1
  stopifnot("`nested` must be a logical vector of length 1." =
              assertthat::is.flag(nested) && assertthat::noNA(nested))

  # 2. Other checks
  # Function f must be one of three cube simulation functions
  do.call(stopifnot, setNames(list(identical(f, simulate_occurrences) ||
                                     identical(f, sample_observations) ||
                                     identical(f, grid_designation)),
                              function_message))
  ### End checks

  ## Select data to map function
  # Retrieve arguments of function
  col_arg_names <- get_function_arguments(f, df)

  # Account for possible extra arguments from custom temporal_function
  if (is.null(extra_arg_names)) {
    col_arg_names_full <- unique(col_arg_names)
  } else {
    col_arg_names_full <- unique(extra_arg_names, col_arg_names)
  }

  # Only select names
  selection_names <- intersect(names(df), col_arg_names_full)

  # Select correct data for mapping
  analysis_df <- dplyr::select(df, all_of(selection_names))

  ## Create output dataframe
  # Iterate function over rows
  out_df <- df %>%
    dplyr::mutate(mapped_col = purrr::pmap(analysis_df, f))

  # Unnest if specified
  if (!nested) {
    out_df <- out_df %>%
      tidyr::unnest(cols = "mapped_col")
  }

  return(out_df)
}
