#' Map `grid_designation()` over multiple species
#'
#' This function executes `grid_designation()` over multiple rows of a
#' dataframe, representing different species, with potentially
#' different function arguments over multiple columns.
#'
#' @param df A dataframe containing multiple rows, each representing a
#' different species. The columns are function arguments with values used for
#' mapping `grid_designation()` for each species. Columns not used by this
#' function will be retained in the output.
#' @param nested Logical. If `TRUE` (default), retains list-column containing
#' sf objects calculated by `grid_designation()`. Otherwise, expands
#' this list-column into rows and columns.
#' @param arg_list A named list or `NA`. If `NA` (default), the function assumes
#' column names in `df` are identical to argument names of
#' `grid_designation()`. If column names differ, they must
#' to be specified as a named list where the names are the argument names of
#' `grid_designation()`, and the associated values are the corresponding column
#' names in `df`.
#' @param progress Logical. Whether to show a progress bar. Set
#' to `TRUE` to display a progress bar, `FALSE` (default) to suppress it.
#'
#' @returns In case of `nested = TRUE`, a dataframe identical to `df`, but each
#' sf object with POINT geometry in the list-column
#' `observations` now has an additional column `coordinateUncertaintyInMeters`
#' added by `grid_designation()`. In case of `nested = FALSE`, this
#' list-column is expanded into additional rows and columns.
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
#' \dontrun{
#' # Load packages
#' library(sf)
#' library(dplyr)
#'
#' # Create polygon
#' plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))
#'
#' # Create grid
#' cube_grid <- st_make_grid(
#'     st_buffer(plgn, 25),
#'     n = c(20, 20),
#'     square = TRUE) %>%
#'   st_sf()
#'
#' ## Example with simple column names
#' # Specify dataframe for 3 species with custom function arguments
#' species_dataset_df <- tibble(
#'   taxonID = c("species1", "species2", "species3"),
#'   species_range = rep(list(plgn), 3),
#'   initial_average_occurrences = c(50, 100, 200),
#'   n_time_points = rep(6, 3),
#'   temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
#'   sd_step = c(1, 1, NA),
#'   spatial_pattern = "random",
#'   detection_probability = c(0.8, 0.9, 1),
#'   invert = FALSE,
#'   coords_uncertainty_meters = c(25, 30, 50),
#'   grid = rep(list(cube_grid), 3),
#'   seed = 123)
#'
#' # Simulate occurrences
#' sim_occ1 <- map_simulate_occurrences(df = species_dataset_df)
#'
#' # Sample observations
#' samp_obs1 <- map_sample_observations(df = sim_occ1)
#'
#' # Filter observations
#' filter_obs1 <- map_filter_observations(df = samp_obs1)
#'
#' # Add coordinate uncertainty
#' obs_uncertainty1 <- map_add_coordinate_uncertainty(df = filter_obs1)
#'
#' # Grid designation
#' occ_cube_nested <- map_grid_designation(df = obs_uncertainty1)
#' occ_cube_nested
#'
#' ## Example with deviating column names
#' # Specify dataframe for 3 species with custom function arguments
#' species_dataset_df2 <- species_dataset_df %>%
#'   rename(polygon = species_range,
#'          sd = sd_step,
#'          det_prob = detection_probability,
#'          inv = invert,
#'          coord_uncertainty = coords_uncertainty_meters,
#'          raster = grid)
#'
#' # Create named list for argument conversion
#' arg_conv_list <- list(
#'     species_range = "polygon",
#'     sd_step = "sd",
#'     detection_probability = "det_prob",
#'     invert = "inv",
#'     coords_uncertainty_meters = "coord_uncertainty",
#'     grid = "raster"
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
#' filter_obs2 <- map_filter_observations(
#'   df = samp_obs2,
#'   arg_list = arg_conv_list)
#'
#' # Add coordinate uncertainty
#' obs_uncertainty2 <- map_add_coordinate_uncertainty(
#'   df = filter_obs2,
#'   arg_list = arg_conv_list)
#'
#' # Grid designation
#' map_grid_designation(
#'   df = obs_uncertainty2,
#'   arg_list = arg_conv_list)
#' }

map_grid_designation <- function(
    df,
    nested = TRUE,
    arg_list = NA,
    progress = FALSE) {
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
    f = grid_designation,
    df = df,
    nested = nested,
    progress = progress
  )

  # Rename columns
  colnames(out_df)[seq_along(og_colnames)] <- og_colnames

  if (nested) {
    out_df <- out_df %>%
      dplyr::rename("occurrence_cube_df" = "mapped_col")
  }

  return(out_df)
}
