#' Map cube simulation functions over multiple rows of a dataframe
#'
#' The function executes a cube simulation function (`simulate_occurrences()`,
#' `sample_observations()`, `filter_observations()`,
#' `add_coordinate_uncertainty()`, or `grid_designation()`) over multiple rows
#' of a dataframe containing potentially different function arguments over
#' multiple columns.
#'
#' @param f One of five cube simulation functions: `simulate_occurrences()`,
#' `sample_observations()`, `filter_observations()`,
#' `add_coordinate_uncertainty()`, or `grid_designation()`.
#' @param df A dataframe containing multiple rows. Each row is considered a
#' different species. The columns are function arguments with values used for
#' mapping `f` for each species. `df` can have columns that are not used by this
#' function. They will be retained in the output.
#' @param nested Logical. If `TRUE` (default), retain list-column containing
#' dataframes calculated by `f`. Otherwise, expand this list-column into rows
#' and columns.
#'
#' @returns In case of `nested = TRUE`, a dataframe identical to the input
#' dataframe `df`, but with an extra list-column called `mapped_col` containing
#' an sf object for each row computed by the function specified in `f`. In case
#' of `nested = FALSE`, this list-column is expanded into additional rows and
#' columns.
#'
#' @export
#'
#' @import dplyr
#' @importFrom purrr pmap quietly
#' @importFrom tidyr unnest
#' @importFrom stats setNames
#'
#' @family multispecies_low
#'
#' @examples
#' # Load packages
#' library(sf)
#' library(dplyr)
#'
#' # Create polygon
#' plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))
#'
#' ## Example with simple column names
#' # Specify dataframe for 3 species with custom function arguments
#' species_dataset_df <- tibble(
#'   taxonID = c("species1", "species2", "species3"),
#'   plgn = rep(list(plgn), 3),
#'   initial_average_abundance = c(50, 100, 500),
#'   n_time_points = rep(6, 3),
#'   temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
#'   sd_step = c(1, 1, NA),
#'   spatial_autocorr = "random",
#'   seed = 123)
#'
#' # Simulate occurrences
#' sim_occ_raw <- map_simulation_functions(
#'   f = simulate_occurrences,
#'   df = species_dataset_df)
#' sim_occ_raw
#'
#' # Unnest output and create sf object
#' sim_occ_raw_unnested <- map_simulation_functions(
#'   f = simulate_occurrences,
#'   df = species_dataset_df,
#'   nested = FALSE)
#'
#' sim_occ_raw_unnested %>%
#'    st_sf()

map_simulation_functions <- function(
    f,
    df,
    nested = TRUE) {
  ### Start checks
  # 1. Check input type and length
  # Check if f is a function
  function_message <- paste("`f` must be a function.\nOne of",
                            "`simulate_occurrences()`,",
                            "`sample_observations()`,",
                            "`filter_observations()`",
                            "`add_coordinate_uncertainty()`,",
                            "or `grid_designation()`.")
  do.call(stopifnot, stats::setNames(list(inherits(f, "function")),
                                     function_message))

  # Check if df is a dataframe
  stopifnot("`df` must be a dataframe." = inherits(df, "data.frame"))

  # Check if nested is a logical vector of length 1
  stopifnot("`nested` must be a logical vector of length 1." =
              assertthat::is.flag(nested) && assertthat::noNA(nested))

  # 2. Other checks
  # Function f must be one of three cube simulation functions
  do.call(stopifnot, stats::setNames(
        list(
          identical(f, simulate_occurrences) ||
           identical(f, sample_observations) ||
           identical(f, filter_observations) ||
           identical(f, add_coordinate_uncertainty) ||
           identical(f, grid_designation)),
        function_message
      )
    )
  ### End checks

  ## Select data to map function
  # Retrieve arguments of function
  col_arg_names <- get_function_arguments(f, df)

  # Only select names
  selection_names <- intersect(names(df), col_arg_names)

  # Select correct data for mapping
  analysis_df <- dplyr::select(df, all_of(selection_names))

  ## Create output dataframe
  # Iterate function over rows and catch warnings
  mapped_df <- df %>%
    dplyr::mutate(mapped_col = purrr::pmap(.l = analysis_df,
                                           .f = purrr::quietly(f),
                                           .progress = TRUE))

  # Handle potential warnings
  out_df <- handle_mapped_warnings(mapped_df)

  # Handle potential messages
  messages <- sapply(mapped_df$mapped_col, function(i) i$output)
  if (length(messages[nzchar(messages)]) > 0) print(messages, quote = FALSE)

  # Unnest if specified
  if (!nested) {
    out_df <- out_df %>%
      tidyr::unnest(cols = "mapped_col", names_repair = "minimal")
    out_df <- out_df[, !duplicated(t(out_df))]
  }

  return(out_df)
}
