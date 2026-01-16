#' Simulate species occurrences within a spatiotemporal scope
#'
#' This function simulates occurrences of a species within a specified spatial
#' and/or temporal extent.
#'
#' @param species_range An sf object with POLYGON geometry indicating the
#' spatial extent to simulate occurrences.
#' @param initial_average_occurrences A positive numeric value indicating the
#' average number of occurrences to be simulated within the extent of
#' `species_range` at the first time point. This value serves as the mean
#' (lambda) of a Poisson distribution.
#' @param n_time_points A positive integer specifying the number of time points
#' to simulate.
#' @param temporal_function A function generating a trend in number of
#' occurrences over time, or `NA` (default). If `n_time_points` > 1 and a
#' function is provided, it defines the temporal pattern of number of
#' occurrences.
#' @param ... Additional arguments to be passed to `temporal_function`.
#' @param spatial_pattern Specifies the spatial pattern of occurrences. It can
#' be a character string (`"random"` or `"clustered"`) or a numeric value ≥ 1
#' (1 means random distribution, larger values indicate more clustering).
#' The default is `"random"`. `"clustered"` corresponds to a value of 10.
#' See `create_spatial_pattern()`.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), then `set.seed()`
#' is not called at all. If not `NA`, then the random number generator state is
#' reset (to the state before calling this function) upon exiting this function.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' simulated occurrences, a `time_point` column indicating the associated
#' time point for each occurrence and a `sampling_p1` column indicating the
#' sampling probability associated with the spatial pattern (see
#' `create_spatial_pattern()`).
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
#'
#' # 1. Random spatial pattern with 4 time points
#' occ_sf <- simulate_occurrences(
#'   species_range = plgn,
#'   n_time_points = 4,
#'   initial_average_occurrences = 100,
#'   seed = 123)
#'
#' ggplot() +
#'  geom_sf(data = occ_sf) +
#'  geom_sf(data = plgn, fill = NA) +
#'  facet_wrap("time_point") +
#'  labs(
#'       title = "Occurrences with random spatial and temporal pattern",
#'       subtitle = "4 time steps") +
#'  theme_minimal()
#'
#' # 2. Highly clustered spatial pattern with 6 time points
#' occ_sf_100 <- simulate_occurrences(
#'   species_range = plgn,
#'   spatial_pattern = 100,
#'   n_time_points = 6,
#'   initial_average_occurrences = 100,
#'   seed = 123)
#'
#' ggplot() +
#'   geom_sf(data = occ_sf_100) +
#'   geom_sf(data = plgn, fill = NA) +
#'   facet_wrap("time_point") +
#'   labs(
#'        title = "Occurrences with structured spatial and temporal pattern",
#'        subtitle = "6 time steps") +
#'   theme_minimal()

simulate_occurrences <- function(
    species_range,
    initial_average_occurrences = 50,
    spatial_pattern = c("random", "clustered"),
    n_time_points = 1,
    temporal_function = NA,
    ...,
    seed = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if species_range is an sf object
  stopifnot(
    "`species_range` must be an sf object with POLYGON geometry." =
      inherits(species_range, "POLYGON") |
      inherits(species_range, "sfc_POLYGON") |
      (inherits(species_range, "sf") &&
       sf::st_geometry_type(species_range) == "POLYGON")
  )

  # Check if initial_average_occurrences is a positive number
  stopifnot(
    "`initial_average_occurrences` must be a single positive number." =
      assertthat::is.number(initial_average_occurrences) &
      initial_average_occurrences >= 0
  )

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

  # Check if n_time_points is a positive integer
  stopifnot("`n_time_points` must be a single positive integer." =
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
    seed = seed
  )

  # Create the random field
  box_plgn <- sf::st_bbox(species_range)
  species_range_maxr <- max(box_plgn[3] - box_plgn[1],
                            box_plgn[4] - box_plgn[2])
  res <- species_range_maxr / 100

  rs_pattern <- create_spatial_pattern(
    polygon = species_range,
    resolution = res,
    spatial_pattern = spatial_pattern,
    seed = seed,
    n_sim = 1
  )

  # Sample occurrences from raster
  occ <- sample_occurrences_from_raster(
    raster = rs_pattern,
    time_series = ts,
    seed = seed
  )

  # Return the occurences (sf point geometry)
  return(occ)
}
