#' Sample occurrences from spatial random field
#'
#' Draws occurrences (points) from a spatial random field (raster)
#'
#' @param rs A raster object (terra).
#' @param ts A vector with the number of occurrences per time point.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @return An sf object with POINT geometry
#'
#' @export
#'
#' @import sf
#' @importFrom terra spatSample global
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
#' ggplot() +
#'   geom_sf(data = plgn) +
#'   theme_minimal()
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
#'   rs = rs_pattern_clustered,
#'   ts = 200,
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
#'   rs = rs_pattern_large,
#'   ts = 200,
#'   seed = 123)
#'
#' ggplot() +
#'   geom_spatraster(data = rs_pattern_large) +
#'   geom_sf(data = pts_occ_large) +
#'   scale_fill_continuous(type = "viridis") +
#'   theme_minimal()

sample_occurrences_from_raster <- function(
    rs,
    ts,
    seed = NA) {
  # checks
  # check if rs is a terra raster
  if (!"SpatRaster" %in% class(rs)) {
    cli::cli_abort(c("{.var rs} is not a SpatRaster."))
  }

  # check if ts is a numeric vector
  if (!is.numeric(ts)) {
    cli::cli_abort(c("{.var ts} must be an numeric vector"))
  }

  # check if seed is a single value
  if (length(seed) != 1) {
    cli::cli_abort(c(
      "{.var seed} must be a numeric vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(seed)}} vector",
        "of length {length(seed)}."
      )
    ))
  }

  # centre the values of the raster (mean = 0)
  rs_mean <- terra::global(rs, "mean", na.rm = TRUE)[, 1]
  rs2 <- rs - rs_mean

  # increase contrast between high and low values
  a <- 30 # a = 1 -> logistic  a > 1  => steeper sigmoid (higher contrast)
  rs3 <- 1 / (1 + exp(-a * rs2))

  # For each time step sample points from the raster
  # Should be recoded: with lapply? or map?

  occ_pf <- NULL

  # Set seed if provided
  if (!is.na(seed)) {
    if (is.numeric(seed)) {
      withr::local_seed(seed)
    } else {
      cli::cli_abort(c(
        "{.var seed} must be a numeric vector of length 1.",
        "x" = paste(
          "You've supplied a {.cls {class(seed)}} vector",
          "of length {length(seed)}."
        )
      ))
    }
  }

  for (t in seq_along(ts)) {
    occ_p <- terra::spatSample(
      x = rs3, size = ts[t], method = "weights",
      replace = TRUE, as.points = TRUE
    )
    occ_sf <- sf::st_as_sf(occ_p)
    occ_sf$time_point <- t
    occ_pf <- rbind(occ_pf, occ_sf)
  }

  # points need to be shifted randomly (uniform within the raster cell size)
  # For the moment the points are all at the center of the raster cells

  return(occ_pf)
}
