#' Simulate species occurrences within a spatiotemporal scope
#'
#' This function simulates occurrences of a species within a specified spatial
#' and/or temporal extent.
#'
#' @param plgn An sf object with POLYGON geometry indicating the spatial
#' extend to simulate occurrences.
#' @param initial_average_occurrences A positive numeric value indicating the
#' average number of occurrences to be simulated within the extent of `plgn`
#' at the first time point. This value serves as the mean (lambda) of a Poisson
#' distribution.
#' @param n_time_points A positive integer specifying the number of time points
#' to simulate.
#' @param temporal_function A function generating a trend in number of
#' occurrences over time, or `NA` (default). If `n_time_points` > 1 and a
#' function is provided, it defines the temporal pattern of number of
#' occurrences.
#' @param ... Additional arguments to be passed to `temporal_function`.
#' @param spatial_autocorr Specifies the spatial pattern of occurrences. It can
#' be a character string (`"random"` or `"clustered"`) or a numeric value ≥ 1
#' (1 means random distribution, larger values indicate more clustering).
#' The default is `"random"`. `"clustered"` corresponds to a value of 10.
#' See `create_spatial_pattern()`.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), no seed is used.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' simulated occurrences and a `time_point` column indicating the associated
#' time point for each occurrence.
#'
#' @export
#'
#' @import assertthat
#' @import sf
#'
#' @family main
#'
#' @examples
#' # Load packages
#' library(sf)
#' library(ggplot2)
#'
#' # Create polygon
#' plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))
#' ggplot() +
#'   geom_sf(data = plgn) +
#'   theme_minimal()
#'
#' ## Random spatial pattern with 4 time points
#' occ_sf <- simulate_occurrences(
#'   plgn,
#'   n_time_points = 4,
#'   initial_average_occurrences = 100,
#'   seed = 123)
#'
#' ggplot() +
#'  geom_sf(data = occ_sf) +
#'  geom_sf(data = plgn, fill = NA) +
#'  facet_wrap("time_point") +
#'  labs(
#'       title = "Occurrences with random\nspatial and temporal pattern",
#'       subtitle = "4 time steps") +
#'  theme_bw()
#'
#' ## Clustered spatial pattern with 4 time points
#' occ_sf_100 <- simulate_occurrences(
#'   plgn,
#'   spatial_autocorr = 100,
#'   n_time_points = 4,
#'   initial_average_occurrences = 100,
#'   seed = 123)
#'
#' ggplot() +
#'   geom_sf(data = occ_sf_100) +
#'   geom_sf(data = plgn, fill = NA) +
#'   facet_wrap("time_point") +
#'   labs(
#'        title = "Occurrences with structured\nspatial and temporal pattern",
#'        subtitle = "4 time steps") +
#'   theme_bw()

simulate_occurrences <- function(
    plgn,
    initial_average_occurrences = 50,
    spatial_autocorr = c("random", "clustered"),
    n_time_points = 1,
    temporal_function = NA,
    ...,
    seed = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if plgn is an sf object
  stopifnot("`plgn` must be an sf object with POLYGON geometry." =
              inherits(plgn, "POLYGON") | inherits(plgn, "sfc_POLYGON") |
              (inherits(plgn, "sf") && sf::st_geometry_type(plgn) == "POLYGON"))

  # Check if initial_average_occurrences is a positive number
  stopifnot(
    "`initial_average_occurrences` must be a single positive number." =
      assertthat::is.number(initial_average_occurrences) &
      initial_average_occurrences >= 0)

  if (!(assertthat::is.number(spatial_autocorr) && spatial_autocorr >= 1)) {
    # Check if spatial_autocorr is random or clustered
    spatial_autocorr <- tryCatch({
      match.arg(spatial_autocorr, c("random", "clustered"))
    }, error = function(e) {
      stop(paste0("`spatial_autocorr` must be one of 'random', 'clustered',",
                  " or a single number larger or equal to 1."),
           call. = FALSE)
    })
  }

  # Check if n_time_points is a positive integer
  stopifnot(
    "`n_time_points` must be a single positive integer." =
      assertthat::is.count(n_time_points))

  # Check if temporal_function is NA or a function
  stopifnot("`temporal_function` must be `NA` or a function." =
              (is.function(temporal_function) || is.na(temporal_function)) &
              length(temporal_function) == 1)

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (assertthat::is.number(seed) | is.na(seed)) &
              length(seed) == 1)
  ### End checks

  # Simulate the timeseries
  ts <- simulate_timeseries(
    initial_average_occurrences = initial_average_occurrences,
    n_time_points = n_time_points,
    temporal_function = temporal_function,
    ...,
    seed = seed)

  # Create the random field
  boxplgn <- sf::st_bbox(plgn)
  plgn_maxr <- max(boxplgn[3] - boxplgn[1], boxplgn[4] - boxplgn[2])
  res <- plgn_maxr / 100

  rs_pattern <- create_spatial_pattern(
    polygon = plgn,
    resolution = res,
    spatial_pattern = spatial_autocorr,
    seed = seed,
    n_sim = 1)

  # Sample occurrences from raster
  occ <- sample_occurrences_from_raster(
    rs = rs_pattern,
    ts = ts,
    seed = seed)

  # Return the occurences (sf point geometry)
  return(occ)
}
