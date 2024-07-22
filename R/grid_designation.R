#' Observations to grid designation to create a data cube
#'
#' The function designates observations to cells of a given grid to create an
#' aggregated data cube.
#'
#' @param observations An sf object with POINT geometry and a `time_point` and
#' `coordinateUncertaintyInMeters` column. If this last column is not present,
#' the function will assume no (zero meters) uncertainty around the observation
#' points.
#' @param grid An sf object with POLYGON geometry (usually a grid) to which
#' observations should be designated.
#' @param id_col The column name of the column with unique ids for each grid
#' cell. If `"row_names"` (the default), a new column `id` is created were the
#' row names represent the unique ids.
#' @param seed The seed for random number generation to make results
#' reproducible. If `NA` (the default), no seed is used.
#' @param aggregate Logical. If `TRUE` (default), return data cube in
#' aggregated form (grid with number of observations per grid cell). Otherwise
#' return sampled points in uncertainty circle.
#' @param randomisation `"uniform"` or `"normal"`. Randomisation method used
#' for sampling within uncertainty circle around each observation. By default
#' `"uniform"` which means each point uncertainty circle has an equal
#' probability to be selected. The other option is `"normal"` where a point is
#' sampled from a bivariate Normal distribution with means equal to the
#' observation point and the variance equal to
#' (-`coordinateUncertaintyInMeters`^2) / (2 * log(1 - `p_norm`)) such that
#' `p_norm` % of all possible samples from this Normal distribution fall
#' within the uncertainty circle.
#' @param p_norm A numeric value between 0 and 1. Only used if
#' `randomisation = "normal"`. The proportion of all possible samples from a a
#' bivariate Normal distribution that fall within the uncertainty circle. If
#' normal randomisation is used and no value is given, the default `p_norm`
#' value is 0.95.
#'
#' @returns In case of `aggregate = TRUE`, an sf object with POLYGON geometry
#' containing the locations of the grid cells, an `n` column with the number of
#' observations per grid cell, and a `min_coord_uncertainty` column containing
#' the minimal coordinate uncertainty per grid cell. In case of
#' `aggregate = FALSE`, an sf object with POINT geometry containing the
#' locations of the sampled observations within the uncertainty circle, and a
#' `coordinateUncertaintyInMeters` column containing the coordinate uncertainty
#' for each observation.
#'
#' @export
#'
#' @import dplyr
#' @import sf
#' @import assertthat
#' @importFrom rlang .data
#' @importFrom cli cli_abort cli_warn
#'
#' @family main
#'
#' @examples
#' library(sf)
#' library(dplyr)
#'
#' set.seed(123)
#'
#' # Create four random points
#' n_points <- 4
#' xlim <- c(3841000, 3842000)
#' ylim <- c(3110000, 3112000)
#' coordinate_uncertainty <- rgamma(n_points, shape = 5, rate = 0.1)
#'
#' observations_sf <- data.frame(
#'   lat = runif(n_points, ylim[1], ylim[2]),
#'   long = runif(n_points, xlim[1], xlim[2]),
#'   time_point = 1,
#'   coordinateUncertaintyInMeters = coordinate_uncertainty
#' ) %>%
#'   st_as_sf(coords = c("long", "lat"), crs = 3035)
#'
#' # Add buffer uncertainty in meters around points
#' observations_buffered <- observations_sf %>%
#'   st_buffer(observations_sf$coordinateUncertaintyInMeters)
#'
#' # Create grid
#' grid_df <- st_make_grid(
#'   observations_buffered,
#'   square = TRUE,
#'   cellsize = c(200, 200)
#' ) %>%
#'   st_sf()
#'
#' # Create occurrence cube
#' grid_designation(
#'   observations = observations_sf,
#'   grid = grid_df,
#'   seed = 123
#' )

grid_designation <- function(
    observations,
    grid,
    id_col = "row_names",
    seed = NA,
    aggregate = TRUE,
    randomisation = c("uniform", "normal"),
    p_norm = ifelse(tolower(randomisation[1]) == "uniform", NA, 0.95)) {
  ### Start checks
  # 1. Check input type and length
  # Check if observations is an sf object
  stopifnot("`observations` must be an sf object." =
              inherits(observations, "sf"))

  # Check if grid is an sf object
  stopifnot("`grid` must be an sf object." =
              inherits(grid, "sf"))

  # Check if id_col is a character vector of length 1
  stopifnot("`id_col` must be a character vector of length 1." =
              assertthat::is.string(id_col))

  # Check if aggregate is a logical vector of length 1
  stopifnot("`aggregate` must be a logical vector of length 1." =
              assertthat::is.flag(aggregate) & assertthat::noNA(aggregate))

  # Check if randomisation is uniform or normal
  randomisation <- tryCatch({
    match.arg(randomisation, c("uniform", "normal"))
    }, error = function(e) {
      stop("`randomisation` must be one of 'uniform', 'normal'.",
           call. = FALSE)
  })

  # 2. Other checks
  # CRS of sf objects
  stopifnot("`grid` must have the same CRS as `observations`." =
              sf::st_crs(observations) == sf::st_crs(grid))

  # Unique ids if id column is provided
  if (id_col != "row_names") {
    if (!id_col %in% names(grid)) {
      warning(
        paste0(
          "Column name '",  id_col, "' not present in provided grid!\n",
          "Creating ids based on row names."
          )
        )
      id_col <- "row_names"
    } else if (length(unique(grid[[id_col]])) != nrow(grid)) {
      warning(
        paste0(
          "Column '",  id_col, "' does not contain unique ids for grid cells!",
          "\nCreating new ids based on row names."
        )
      )
      id_col <- "row_names"
    }
  }
  ### End checks

  # Get random point in uncertainty circle according to uniform or normal rules
  if (randomisation == "uniform") {
    new_points <- sample_from_uniform_circle(observations, seed)
  } else {
    new_points <- sample_from_binormal_circle(observations, p_norm, seed)
  }

  # We assign each occurrence to a grid cell
  # Each grid cell needs a unique id
  if (id_col == "row_names") {
    id_col <- "id"
    grid[[id_col]] <- rownames(grid)
  }
  sf::st_agr(new_points) <- "constant"
  sf::st_agr(grid) <- "constant"
  intersect_grid <- sf::st_intersection(new_points, grid)

  # Return object
  if (aggregate) {
    # Aggregate to get the cube
    occ_cube_df <- intersect_grid %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by_at(c("time_point", id_col)) %>%
      dplyr::summarise(
        n = dplyr::n(),
        min_coord_uncertainty = min(.data$coordinateUncertaintyInMeters)
      ) %>%
      dplyr::ungroup()

    # Add zeroes for each time point
    design <- expand.grid(time_point = unique(occ_cube_df$time_point),
                          id_col = unique(grid[[id_col]])) %>%
      dplyr::rename_with(~id_col, id_col) %>%
      dplyr::full_join(grid, by = dplyr::join_by(!!id_col))

    out_sf <- occ_cube_df %>%
      dplyr::full_join(design, by = c(id_col, "time_point")) %>%
      dplyr::mutate(n = as.integer(ifelse(is.na(n), 0, n))) %>%
      sf::st_as_sf(crs = sf::st_crs(grid))
  } else {
    # Return new points
    out_sf <- intersect_grid %>%
      dplyr::select_at(c(id_col, "time_point", "coordinateUncertaintyInMeters"))
  }

  return(out_sf)
}
