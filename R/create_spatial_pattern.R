#' Create spatial pattern within a polygon
#'
#' This function creates a raster with a spatial pattern for the area of a
#' polygon.
#'
#' @param polygon An sf object with POLYGON geometry.
#' @param resolution A numeric value defining the resolution of the raster cell.
#' @param spatial_pattern Specifies the desired spatial pattern. It can
#' be a character string (`"random"` or `"clustered"`) or a numeric value ≥ 1
#' (1 means random distribution, larger values indicate more clustering).
#' The default is `"random"`. `"clustered"` corresponds to a value of 10.
#' See details.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), no seed is used.
#' @param n_sim Number of simulations. Each simulation is a different layer in
#' the raster. Default is 1.
#'
#' @details The `spatial_pattern` argument changes the range parameter of the
#' spherical variogram model. `spatial_pattern = 1` means the range has the same
#' size as the grid cell, which is defined in the `resolution` argument. The
#' function [gstat::vgm()] is used to implement the spherical variogram model.
#'
#' @seealso [gstat::vgm()] and its `range` argument
#'
#' @return An object of class SpatRaster with a spatial pattern for the area of
#' the given polygon.
#'
#' @export
#'
#' @import sf
#' @import dplyr
#' @import assertthat
#' @importFrom stats predict
#' @importFrom terra vect rast rasterize
#' @importFrom gstat vgm gstat
#' @importFrom withr local_seed
#' @importFrom vegan decostand
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
#' # Random spatial pattern
#' rs_pattern_random <- create_spatial_pattern(
#'   polygon = plgn,
#'   resolution = 0.1,
#'   spatial_pattern = "random",
#'   seed = 123)
#'
#' ggplot() +
#'   geom_spatraster(data = rs_pattern_random) +
#'   scale_fill_continuous(type = "viridis") +
#'   theme_minimal()
#'
#' ## Clustered spatial pattern
#' rs_pattern_clustered <- create_spatial_pattern(
#'   polygon = plgn,
#'   resolution = 0.1,
#'   spatial_pattern = "clustered",
#'   seed = 123)
#'
#' ggplot() +
#'   geom_spatraster(data = rs_pattern_clustered) +
#'   scale_fill_continuous(type = "viridis") +
#'   theme_minimal()
#'
#' ## User defined spatial pattern
#' # Small scale clustering
#' rs_pattern_small <- create_spatial_pattern(
#'   polygon = plgn,
#'   resolution = 0.1,
#'   spatial_pattern = 5,
#'   seed = 123)
#'
#' ggplot() +
#'   geom_spatraster(data = rs_pattern_small) +
#'   scale_fill_continuous(type = "viridis") +
#'   theme_minimal()
#'
#' # Medium scale clustering (= the built-in clustered pattern)
#' rs_pattern_medium <- create_spatial_pattern(
#'   polygon = plgn,
#'   resolution = 0.1,
#'   spatial_pattern = 10,
#'   seed = 123)
#'
#' ggplot() +
#'   geom_spatraster(data = rs_pattern_medium) +
#'   scale_fill_continuous(type = "viridis") +
#'   theme_minimal()
#'
#' # Large scale clustering
#' rs_pattern_large <- create_spatial_pattern(
#'   polygon = plgn,
#'   resolution = 0.1,
#'   spatial_pattern = 100,
#'   seed = 123)
#'
#' ggplot() +
#'   geom_spatraster(data = rs_pattern_large) +
#'   scale_fill_continuous(type = "viridis") +
#'   theme_minimal()

create_spatial_pattern <- function(
    polygon,
    resolution,
    spatial_pattern = c("random", "clustered"),
    seed = NA,
    n_sim = 1
  ) {
  ### Start checks
  # 1. Check input type and length
  # Check if polygon is an sf object
  stopifnot("`polygon` must be an sf object with POLYGON geometry." =
              inherits(polygon, "POLYGON") | inherits(polygon, "sfc_POLYGON") |
              (inherits(polygon, "sf") &&
                 sf::st_geometry_type(polygon) == "POLYGON"))

  if (!(assertthat::is.number(spatial_pattern) && spatial_pattern >= 1)) {
    # Check if spatial_pattern is random or clustered
    spatial_pattern <- tryCatch({
      match.arg(spatial_pattern, c("random", "clustered"))
    }, error = function(e) {
      stop(paste0("`spatial_pattern` must be one of 'random', 'clustered',",
                  " or a single number larger or equal to 1."),
           call. = FALSE)
    })
  }

  # Check if resolution is a positive number
  stopifnot("`resolution` must be a single positive number." =
              assertthat::is.number(resolution) & resolution >= 0)

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (assertthat::is.number(seed) | is.na(seed)) &
              length(seed) == 1)

  # Check if n_sim is a positive integer
  stopifnot(
    "`n_sim` must be a single positive integer." =
      assertthat::is.count(n_sim))
  ### End checks

  # Create a reference raster with same extent as the polygon and user defined
  # resolution
  poly_vect <- terra::vect(polygon)
  templ <- terra::rast(poly_vect, res = resolution)
  poly_raster <- terra::rasterize(poly_vect, templ)

  dfxy <- as.data.frame(poly_raster, xy = TRUE)

  # Define the spatial pattern ----
  if (is.character(spatial_pattern)) {
    if (spatial_pattern == "random") {
      multiplier <- 1
    }
    if (spatial_pattern == "clustered") {
      multiplier <- 10
    }
  } else {
    multiplier <- spatial_pattern
  }

  # Set seed if provided
  if (!is.na(seed)) {
    withr::local_seed(seed)
  }

  # Use gstat object with vgm model to create spatial pattern
  range_size <- resolution * multiplier

  gstat_model <- gstat::gstat(
    formula = z ~ 1,
    locations = ~ x + y,
    dummy = TRUE,
    beta = 1,
    model = gstat::vgm(
      psill = 0.5,
      model = "Sph",
      range = range_size,
      nugget = 0),
    nmax = 2)

  # Predict pattern based on vgm model
  dfxy_pred <- stats::predict(gstat_model, newdata = dfxy, nsim = n_sim)

  # Standardise values between 0 and 1
  dfxy_std <- dfxy_pred %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("sim"),
        ~vegan::decostand(.x, "range")
        )
    )

  # Return final raster
  return(terra::rast(dfxy_std, crs = terra::crs(poly_vect)))
}
