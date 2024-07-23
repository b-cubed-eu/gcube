#' Sample observations from a larger occurrence dataset
#'
#' The function samples observations from occurrences based on detection
#' probability and sampling bias by implementing a Bernoulli trial.
#'
#' @param occurrences An sf object with POINT geometry.
#' @param detection_probability A numeric value between 0 and 1, corresponding
#' to the probability of detection of the species.
#' @param sampling_bias `"no_bias"`, `"polygon"` or `"manual"`. The method used
#' to generate a sampling bias. `"polygon"`: bias the sampling in a polygon.
#' Provide your polygon to `bias_area`. Provide bias strength to
#' `bias_strength`. `"manual"`: bias the sampling manually via a raster.
#' Provide your raster layer in which each cell contains the probability to be
#' sampled to `bias_weights`.
#' @param bias_area `NA` or an sf object with POLYGON geometry. Only used if
#' `sampling_bias = "polygon"`. The area in which the sampling will be biased.
#' @param bias_strength `NA` or a positive numeric value. Only used if
#' `sampling_bias = "polygon"`. The strength of the bias to be applied in the
#' biased area (as a multiplier). Above 1, area will be oversampled. Below 1,
#' area will be undersampled. For example, a value of 50 will result in a 50
#' times sampling probability within the `bias_area` than outside. Conversely,
#' a value of 0.5 will result in half less samples within the `bias_area` than
#' outside.
#' @param bias_weights `NA` or a raster layer (sf object with POLYGON geometry).
#' Only used if `sampling_bias = "manual"`. The raster of bias weights to be
#' applied to the sampling of occurrences. Higher weights mean a higher
#' probability of sampling. Weights can be numeric values between 0 and 1 or
#' positive integers that will be rescaled to values between 0 and 1.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' sampled observations, a `detection_probability` column containing the
#' detection probability for each observation (will be the same for all), a
#' `bias_weight` column containing the sampling probability based on sampling
#' bias, a `sampling_probability` column containing the combined sampling
#' probability from detection probability and sampling bias for each
#' observation, and a `sampling_status` column indicating whether the
#' occurrence was detected (observations) or not (unobserved occurrences).
#'
#' @export
#'
#' @import dplyr
#' @import assertthat
#' @importFrom stats rbinom
#' @importFrom withr local_seed
#' @importFrom rlang .data
#'
#' @family main
#'
#' @examples
#' # Load packages
#' library(sf)
#' library(dplyr)
#'
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Simulate some occurrence data with coordinates and time points
#' num_points <- 10
#' occurrences <- data.frame(
#'   lon = runif(num_points, min = -180, max = 180),
#'   lat = runif(num_points, min = -90, max = 90),
#'   time_point = 0
#'   )
#'
#' # Convert the occurrence data to an sf object
#' occurrences_sf <- st_as_sf(occurrences, coords = c("lon", "lat"))
#'
#' # Sample observations without sampling bias
#' sample_observations(
#'   occurrences_sf,
#'   detection_probability = 0.8,
#'   sampling_bias = "no_bias",
#'   seed = 123
#'   )
#'
#' # Sample observations with sampling bias in a polygon
#' # Create bias_area polygon overlapping two of the points
#' selected_observations <- st_union(occurrences_sf[2:3,])
#' bias_area <- st_convex_hull(selected_observations) %>%
#'   st_buffer(dist = 100) %>%
#'   st_as_sf()
#'
#' sample_observations(
#'   occurrences_sf,
#'   detection_probability = 0.8,
#'   sampling_bias = "polygon",
#'   bias_area = bias_area,
#'   bias_strength = 2,
#'   seed = 123
#'   )
#'
#' # Sample observations with sampling bias given manually in a grid
#' # Create raster grid with bias weights between 0 and 1
#' grid <- st_make_grid(occurrences_sf) %>%
#'   st_sf() %>%
#'   mutate(bias_weight = runif(n(), min = 0, max = 1))
#'
#' sample_observations(
#'   occurrences_sf,
#'   detection_probability = 0.8,
#'   sampling_bias = "manual",
#'   bias_weights = grid,
#'   seed = 123
#'   )

sample_observations <- function(
    occurrences,
    detection_probability = 1,
    sampling_bias = c("no_bias", "polygon", "manual"),
    bias_area = NA,
    bias_strength = 1,
    bias_weights = NA,
    seed = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if occurrences is an sf object
  stopifnot("`occurrences` must be an sf object." =
              inherits(occurrences, "sf") &&
              sf::st_geometry_type(occurrences,
                                   by_geometry = FALSE) == "POINT")

  # detection_probability should be numeric between 0 and 1
  stopifnot(
    "`detection_probability` must be a numeric value between 0 and 1." =
      assertthat::is.number(detection_probability) &
      (detection_probability >= 0 & detection_probability <= 1))

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (assertthat::is.number(seed) | is.na(seed)) &
              length(seed) == 1)

  # 2. other checks
  # sampling_bias arguments must match
  sampling_bias <- tryCatch({
    match.arg(sampling_bias, c("no_bias", "polygon", "manual"))
    }, error = function(e) {
      stop("`sampling_bias` must be one of 'no_bias', 'polygon', 'manual'.",
           call. = FALSE)
  })
  ### End checks

  # Set seed if provided
  if (!is.na(seed)) {
    withr::local_seed(seed)
  }

  # Add detection probability
  occurrences$detection_probability <- detection_probability

  # Create and merge bias weights with occurrences
  if (sampling_bias == "polygon") {
    occurrences <- apply_polygon_sampling_bias(
      occurrences_sf = occurrences,
      bias_area = bias_area,
      bias_strength = bias_strength)
  } else if (sampling_bias == "manual") {
    occurrences <- apply_manual_sampling_bias(
      occurrences_sf = occurrences,
      bias_weights = bias_weights
    )
  } else {
    occurrences$bias_weight <- 1
  }

  # Combine detection and bias probabilities and sample observations
  occurrences_combi <- occurrences %>%
    dplyr::mutate(
      sampling_probability = .data$detection_probability * .data$bias_weight
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(sampling_status = stats::rbinom(1, 1,
                                                  .data$sampling_probability),
                  sampling_status = ifelse(.data$sampling_status == 1,
                                           "detected", "undetected")) %>%
    dplyr::ungroup() %>%
    dplyr::select("time_point", "detection_probability", "bias_weight",
                  "sampling_probability", "sampling_status", "geometry")

  # Return the observed occurrences
  return(occurrences_combi)
}
