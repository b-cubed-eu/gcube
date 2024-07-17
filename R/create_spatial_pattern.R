#' Create spatial pattern within a polygon
#'
#' It creates a raster with a spatial pattern for the area of a polygon.
#'
#' @param polygon An sf object of geometry type POLYGON
#' @param resolution A numeric value defining the resolution of the raster cell
#' @param spatial_pattern Define the spatial pattern. It could be a character
#'   string `"random"` or `"clustered"`, in which `"random"` is the default.
#'   The user is able to provide a numeric value >= 1 (1 is "random" and
#'   10 is "clustered"). A larger number means a broader size of the clusters
#'   area. See details.
#' @param seed The seed for random number generation to make results
#' reproducible. If `NA` (the default), no seed is used.
#' @param n_sim Number of simulations. Each simulation is a different layer in
#'   the raster. Default 1.
#'
#' @details
#'   the \code{spatial_pattern} argument change the range parameter of the
#'   spherical variogram model. \code{spatial_pattern = 1} means the range has
#'   the same size of the grid cell, which is defined in \code{resolution}
#'   argument. We use the function [gstat::vgm()] to implement the
#'   spherical variogram model
#'
#' @seealso [gstat::vgm()] and its \code{range} argument
#'
#' @return An object of class SpatRaster with a spatial pattern for the area of
#' the given polygon.
#'
#' @export
#'
#' @import sf
#' @import dplyr
#' @importFrom stats predict
#' @importFrom terra vect rast rasterize
#' @importFrom gstat vgm gstat
#' @importFrom cli cli_abort
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
  if (length(seed) != 1) {
    cli::cli_abort(c(
      "{.var seed} must be a numeric vector of length 1.",
      "x" = paste(
        "You've supplied a {.cls {class(seed)}} vector",
        "of length {length(seed)}."
      )
    ))
  }

  # create a reference raster with same extent as the polygon and user defined
  # resolution
  poly_vect <- terra::vect(polygon)
  templ <- terra::rast(poly_vect, res = resolution)
  poly_raster <- terra::rasterize(poly_vect, templ)

  dfxy <- as.data.frame(poly_raster, xy = TRUE)

  # define the spatial pattern ----
  if (is.character(spatial_pattern)) {
    if (spatial_pattern[1] == "random") {
      multiplier <- 1
    }
    if (spatial_pattern[1] == "clustered") {
      multiplier <- 10
    }

    if (!any(spatial_pattern[1] %in% c("random", "clustered"))) {
      cli::cli_abort(
        c(paste(
          "When class of {.var spatial_pattern} is",
          "{.cls {class(spatial_pattern)}} you should provide 'random' or
          'clustered' as string"),
          "x" = "You've provided the string '{spatial_pattern}' in
          {.var spatial_pattern}"
        )
      )
    }

    ## should have a stopper when is not one of the options

  }

  #
  if (is.numeric(spatial_pattern)) {
    # value should be equal or larger than 1
    if (spatial_pattern >= 1) {
      multiplier <- spatial_pattern
    } else {
      cli::cli_abort("")
    }
  }
  ### End checks

  range_size <- resolution * multiplier

  ## generate the pattern

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

  # Use gstat object with vgm model to create spatial pattern
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
  return(terra::rast(dfxy_std))
}
