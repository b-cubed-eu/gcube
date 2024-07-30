#' Filter detected occurrences
#'
#' This function filters observations from all occurrences based on the
#' `sampling_status` column, typically created by the `sample_observations()`
#' function.
#'
#' @param observations_total An sf object with POINT geometry or a simple
#' dataframe with `sampling_status` column containing values `"detected"`.
#' This format is typically created by the `sample_observations()` function.
#' @param invert Logical. If `FALSE` (default), the function filters to retain
#' only `"detected"` occurrences. If `TRUE`, it filters out `"detected"`
#' occurrences and retains all other occurrences.
#'
#' @returns A data frame or an sf object with POINT geometry containing the
#' filtered observations. If `invert = FALSE`, the function returns detected
#' occurrences. If `invert = TRUE`, it returns all other occurrences.
#'
#' @export
#'
#' @import assertthat
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
#' observations_total_sf <- sample_observations(
#'   occurrences_sf,
#'   detection_probability = 0.8,
#'   sampling_bias = "no_bias",
#'   seed = 123
#'   )
#'
#' # Filter detected observations
#' filter_observations(observations_total_sf)
#'
#' # Filter undetected observations
#' filter_observations(observations_total_sf, invert = TRUE)

filter_observations <- function(observations_total, invert = FALSE) {
  ### Start checks
  # Check if observations_total is a dataframe and/or an sf object
  stopifnot("`observations_total` must be an sf object or a dataframe." =
              inherits(observations_total, "sf") ||
              inherits(observations_total, "data.frame"))

  # Check if invert is a logical vector of length 1
  stopifnot("`invert` must be a logical vector of length 1." =
              assertthat::is.flag(invert) && assertthat::noNA(invert))
  ### End checks


  # Filter dataframe
  if (invert) {
    observations_total[observations_total$sampling_status != "detected", ]
  } else {
    observations_total[observations_total$sampling_status == "detected", ]
  }
}
