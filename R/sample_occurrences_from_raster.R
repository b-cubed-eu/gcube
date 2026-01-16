#' Sample occurrences from spatial random field
#'
#' This function draws point occurrences from a spatial random field represented
#' by a raster. Points are sampled based on the values in the raster, with the
#' number of occurrences specified for each time step.
#'
#' @param raster A SpatRaster object (see [terra::rast()]).
#' @param time_series A vector with the number of occurrences per time point.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), then `set.seed()`
#' is not called at all. If not `NA`, then the random number generator state is
#' reset (to the state before calling this function) upon exiting this function.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' simulated occurrences, a `time_point` column indicating the associated
#' time point for each occurrence and columns used as weights for sampling.
#' If the raster is created with `create_spatial_pattern()`, the column
#' `sampling_p1` is used.
#'
#' @export
#'
#' @import sf
#' @import assertthat
#' @importFrom terra spatSample global res
#' @importFrom purrr map
#' @importFrom stats runif
#'
#' @family occurrence
#'
#' @examples
#' # Load packages
#' library(sf)
#' library(ggplot2)
#' library(tidyterra)
#'
#' # Create polygon
#' plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))
#'
#' ## Medium scale clustering
#' # Create the random field
#' rs_pattern_clustered <- create_spatial_pattern(
#'   polygon = plgn,
#'   resolution = 0.1,
#'   spatial_pattern = "clustered",
#'   seed = 123)
#'
#' # Sample 200 occurrences from random field
#' pts_occ_clustered <- sample_occurrences_from_raster(
#'   raster = rs_pattern_clustered,
#'   time_series = 200,
#'   seed = 123)
#'
#' ggplot() +
#'   geom_spatraster(data = rs_pattern_clustered) +
#'   geom_sf(data = pts_occ_clustered) +
#'   scale_fill_continuous(type = "viridis") +
#'   theme_minimal()
#'
#' ## Large scale clustering
#' # Create the random field
#' rs_pattern_large <- create_spatial_pattern(
#'   polygon = plgn,
#'   resolution = 0.1,
#'   spatial_pattern = 100,
#'   seed = 123)
#'
#' # Sample 200 occurrences from random field
#' pts_occ_large <- sample_occurrences_from_raster(
#'   raster = rs_pattern_large,
#'   time_series = 200,
#'   seed = 123)
#'
#' ggplot() +
#'   geom_spatraster(data = rs_pattern_large) +
#'   geom_sf(data = pts_occ_large) +
#'   scale_fill_continuous(type = "viridis") +
#'   theme_minimal()

sample_occurrences_from_raster <- function(
    raster,
    time_series,
    seed = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if raster is a SpatRaster object
  stopifnot("`raster` must be a SpatRaster object." =
              inherits(raster, "SpatRaster"))

  # Check if time_series is a numeric vector
  stopifnot("`time_series` must be a positive numeric vector." =
              is.numeric(time_series))
  stopifnot("`time_series` must be a positive numeric vector." =
              all(time_series >= 0))

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (assertthat::is.number(seed) | is.na(seed)) &
              length(seed) == 1)
  ### End checks

  # Center the values of the raster (mean = 0)
  rs_mean <- terra::global(raster, "mean", na.rm = TRUE)[, 1]
  rs2 <- raster - rs_mean

  # Increase contrast between high and low values
  a <- 30 # a = 1 -> logistic  a > 1  => steeper sigmoid (higher contrast)
  rs3 <- 1 / (1 + exp(-a * rs2))

  # For each time step, sample points from the raster
  # Get raster resolution to determine cell size
  cell_size <- terra::res(rs3)

  # Set seed if provided
  if (!is.na(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_state_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv)) # nolint: object_name_linter
    }
    set.seed(seed)
  }

  occ_pf_list <- lapply(seq_along(time_series), function(t) {
    # Sample points within the raster
    occ_p <- terra::spatSample(
      x = rs3, size = time_series[t], method = "weights",
      replace = TRUE, as.points = TRUE
    )

    # Convert to sf object
    occ_sf <- sf::st_as_sf(occ_p)

    # Random shift within raster cells
    # Assuming coordinates are in two dimensions (x and y)
    occ_sf$geometry <- sf::st_sfc(purrr::map(occ_sf$geometry, function(pt) {
      shift_x <- stats::runif(1, -0.5 * cell_size[1], 0.5 * cell_size[1])
      shift_y <- stats::runif(1, -0.5 * cell_size[2], 0.5 * cell_size[2])

      sf::st_point(c(sf::st_coordinates(pt)[1] + shift_x,
                     sf::st_coordinates(pt)[2] + shift_y))
    }),
    crs = sf::st_crs(occ_sf))

    # Add time_point column
    occ_sf$time_point <- t

    return(occ_sf)
  })
  occ_pf <- do.call(rbind.data.frame, occ_pf_list) %>%
    dplyr::select("time_point", dplyr::everything())

  return(occ_pf)
}
