#' Map `filter_observations()` over multiple species
#'
#' This function executes `filter_observations()` over multiple rows of a
#' dataframe, representing different species, with potentially
#' different function arguments over multiple columns.
#'
#' @param df A dataframe containing multiple rows, each representing a
#' different species. The columns are function arguments with values used for
#' mapping `filter_observations()` for each species. Columns not used by this
#' function will be retained in the output.
#' @param nested Logical. If `TRUE` (default), retains list-column containing
#' sf objects/dataframes calculated by `filter_observations()`. Otherwise,
#' expands this list-column into rows and columns.
#' @param arg_list A named list or `NA`. If `NA` (default), the function assumes
#' column names in `df` are identical to argument names of
#' `filter_observations()`. If column names differ, they must be
#' specified as a named list where the names are the argument names of
#' `filter_observations()`, and the associated values are the corresponding
#' column names in `df`.
#'
#' @returns In case of `nested = TRUE`, a dataframe identical to `df`, with an
#' extra list-column called `observations` containing an sf object with POINT
#' geometry or simple dataframe for each row computed by
#' `filter_observations()`. In case of `nested = FALSE`, this list-column is
#' expanded into additional rows and columns.
#'
#' @export
#'
#' @import dplyr
#' @import assertthat
#' @importFrom stats setNames
#'
#' @family multispecies
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
#'   detection_probability = c(0.8, 0.9, 1),
#'   invert = FALSE,
#'   seed = 123)
#'
#' # Simulate occurrences
#' sim_occ1 <- map_simulate_occurrences(df = species_dataset_df)
#'
#' # Sample observations
#' samp_obs1 <- map_sample_observations(df = sim_occ1)
#'
#' # Filter observations
#' filter_obs_nested <- map_filter_observations(df = samp_obs1)
#' filter_obs_nested
#'
#' # Unnest output and create sf object again
#' filter_obs_unnested <- map_filter_observations(df = samp_obs1,
#'                                                nested = FALSE)
#' filter_obs_unnested %>%
#'    st_sf()
#'
#'
#' ## Example with deviating column names
#' # Specify dataframe for 3 species with custom function arguments
#' species_dataset_df2 <- species_dataset_df %>%
#'   rename(polygon = plgn,
#'          sd = sd_step,
#'          det_prob = detection_probability,
#'          inv = invert)
#'
#' # Create named list for argument conversion
#' arg_conv_list <- list(
#'     plgn = "polygon",
#'     sd_step = "sd",
#'     detection_probability = "det_prob",
#'     invert = "inv"
#'   )
#'
#' # Simulate occurrences
#' sim_occ2 <- map_simulate_occurrences(
#'   df = species_dataset_df2,
#'   arg_list = arg_conv_list)
#'
#' # Sample observations
#' samp_obs2 <- map_sample_observations(
#'   df = sim_occ2,
#'   arg_list = arg_conv_list)
#'
#' # Filter observations
#' map_filter_observations(
#'   df = samp_obs2,
#'   arg_list = arg_conv_list)

map_filter_observations <- function(
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

  # 2. Other checks
  # Check arg_list
  if (assertthat::noNA(arg_list)) {
    # Check if arg_list is a named list with single strings
    stopifnot(
      "`arg_list` must be named list containing one string for each value." =
        is.list(arg_list) &&
        !is.null(names(arg_list)) &&
        all(sapply(arg_list, assertthat::is.string))
    )

    # Check if arg_list is a named list with single strings
    arg_list_message <- paste("You have provided column names in `arg_list`",
                              "that are not present in `df`.")
    do.call(stopifnot,
            stats::setNames(list(all(unlist(arg_list) %in% colnames(df))),
                            arg_list_message))
  }
  ### End checks

  # Store original column names
  og_colnames <- colnames(df)

  # Rename column names if necessary
  if (assertthat::noNA(arg_list)) {
    df <- df %>%
      dplyr::rename(!!!arg_list)
  }

  # Map function over all rows
  out_df <- map_simulation_functions(
    f = filter_observations,
    df = df,
    nested = nested)

  # Rename columns
  colnames(out_df)[seq_along(og_colnames)] <- og_colnames

  if (nested) {
    out_df <- out_df %>%
      dplyr::rename("observations" = "mapped_col")
  }

  return(out_df)
}
