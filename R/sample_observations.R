#' Sample observations from a larger occurrence dataset
#'
#' The function computes observations from occurrences based on detection
#' probability and sampling bias by implementing a Bernoulli trial.
#'
#' @param occurrences An sf object with POINT geometry representing the
#' occurrences.
#' @param detection_probability A numeric value between 0 and 1 representing the
#' probability of detecting the species.
#' @param sampling_bias `"no_bias"`, `"polygon"` or `"manual"`. The method used
#' to generate a sampling bias. `"polygon"`: bias the sampling in a polygon.
#' Provide your polygon to `bias_area`. Provide bias strength to
#' `bias_strength`. `"manual"`: bias the sampling manually via a raster.
#' Provide your raster layer in which each cell contains the probability to be
#' sampled to `bias_weights`.
#' @param sampling_bias A character string specifying the method to generate a
#' sampling bias.
#' Options are `"no_bias"`, `"polygon"`, or `"manual"`.
#' \describe{
#'   \item{`"no_bias"`}{No bias is applied (default).}
#'   \item{`"polygon"`}{Bias the sampling within a polygon. Provide the polygon
#'   to `bias_area` and the bias strength to `bias_strength`.}
#'   \item{`"manual"`}{Bias the sampling manually using a grid. Provide the grid
#'   layer in which each cell contains the probability of being sampled to
#'   `bias_weights`.}
#' }
#' @param bias_area An `sf` object with POLYGON geometry, or `NA`. Only used if
#' `sampling_bias = "polygon"`. This defines the area in which the sampling will
#' be biased.
#' @param bias_strength A positive numeric value, or `NA`. Only used if
#' `sampling_bias = "polygon"`. The value represents the strength of
#' the bias to be applied within the `bias_area`. Values greater than 1 will
#' increase the sampling probability within the polygon relative to outside
#' (oversampling), while values between 0 and 1 will decrease it
#' (undersampling). For instance, a value of 50 will make the probability 50
#' times higher within the `bias_area` compared to outside, whereas a value of
#' 0.5 will make it half as likely.
#' @param bias_weights A grid layer (an sf object with POLYGON geometry), or
#' `NA`. Only used if `sampling_bias = "manual"`. The grid of bias weights to be
#' applied. This sf object should contain a `bias_weight` column with the
#' weights per grid cell. Higher weights increase the probability of sampling.
#' Weights can be numeric values between 0 and 1 or positive integers, which
#' will be rescaled to values between 0 and 1.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), then `set.seed()`
#' is not called at all. If not `NA`, then the random number generator state is
#' reset (to the state before calling this function) upon exiting this function.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' occurrence with detection status. The object includes the following columns:
#' \describe{
#'   \item{`detection_probability`}{The detection probability for each
#'   occurrence (will be the same for all).}
#'   \item{`bias_weight`}{The sampling probability based on sampling bias for
#'   each occurrence.}
#'   \item{`sampling_probability`}{The combined sampling probability from
#'   detection probability and sampling bias for each occurrence.}
#'   \item{`observed`}{Indicates whether the occurrence was detected
#'   (`TRUE`) or not (`FALSE`). Detected occurrences are called
#'   observations.}
#' }
#'
#' @export
#'
#' @import dplyr
#' @import assertthat
#' @importFrom stats rbinom
#' @importFrom rlang .data
#'
#' @family main
#'
#' @examples
#' # Load packages
#' library(sf)
#' library(dplyr)
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
#' # 1. Sample observations without sampling bias
#' sample_observations(
#'   occurrences_sf,
#'   detection_probability = 0.8,
#'   sampling_bias = "no_bias",
#'   seed = 123
#'   )
#'
#' # 2. Sample observations with sampling bias in a polygon
#' # Create bias_area polygon overlapping two of the points
#' selected_observations <- st_union(occurrences_sf[2:3,])
#' bias_area <- st_convex_hull(selected_observations) %>%
#'   st_buffer(dist = 50) %>%
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
#' # 3. Sample observations with sampling bias given manually in a grid
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
  stopifnot("`detection_probability` must be a numeric value between 0 and 1." =
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
  },
  error = function(e) {
    stop("`sampling_bias` must be one of 'no_bias', 'polygon', 'manual'.",
         call. = FALSE)
  })
  ### End checks

  # Set seed if provided
  if (!is.na(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_state_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv)) # nolint: object_name_linter
    }
    set.seed(seed)
  }

  # Add detection probability
  occurrences$detection_probability <- detection_probability

  # Create and merge bias weights with occurrences
  if (sampling_bias == "polygon") {
    occurrences <- apply_polygon_sampling_bias(
      occurrences_sf = occurrences,
      bias_area = bias_area,
      bias_strength = bias_strength
    )
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
    dplyr::mutate(
      sampling_status = stats::rbinom(1, 1, .data$sampling_probability),
      observed = .data$sampling_status == 1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select("time_point", "detection_probability", "bias_weight",
                  "sampling_probability", "observed", "geometry")

  # Return the observed occurrences
  return(occurrences_combi)
}
