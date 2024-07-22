#' Simulate occurrences within a spatiotemporal scope
#'
#' The function simulates occurrences of a species within a given spatial
#' and/or temporal extend.
#'
#' @param plgn An sf object with POLYGON geometry indicating the spatial
#' extend to simulate occurrences.
#' @param initial_average_abundance A positive numeric value indicating the
#' average number of occurrences to be simulated within the extend of `polygon`
#' at time point 1. This value will be used as mean of a Poisson distribution
#' (lambda parameter).
#' @param spatial_autocorr Define the spatial pattern. It could be a character
#'   string `"random"` or `"clustered"`, in which `"random"` is the default.
#'   The user is able to provide a numeric value >= 1 (1 is "random" and
#'   10 is "clustered"). A larger number means a broader size of the clusters
#'   area. See details.
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param temporal_function `NA` (default), or a function which generates
#' a trend in abundance over time. Only used if `n_time_points > 1`. By default,
#' the function will sample `n_time_points` times from a Poisson
#' distribution with average (lambda) `initial_average_occurrences`. When a
#' function is specified (e.g. the internal `simulate_random_walk()` function).
#' @param ... Additional argument to be passed to the `temporal_function`
#' function.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' simulated occurrences and a `time_point` column containing the time point
#' associated with each occurrence.
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
#'   initial_average_abundance = 100,
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
#'   initial_average_abundance = 100,
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
    initial_average_abundance = 50,
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
              (inherits(plgn, "sf") && st_geometry_type(plgn) == "POLYGON"))

  # Check if initial_average_occurrences is a positive number
  stopifnot(
    "`initial_average_abundance` must be a single positive number." =
      assertthat::is.number(initial_average_abundance) &
      initial_average_abundance >= 0)

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
    initial_average_occurrences = initial_average_abundance,
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
