#' Add coordinate uncertainty to observations
#'
#' This function adds a column to the input dataframe or sf object containing
#' the coordinate uncertainty for each observation, measured in meters.
#'
#' @param observations An sf object with POINT geometry or a simple
#' dataframe representing the observations. This object contains the observation
#' points to which the coordinate uncertainty will be added.
#' @param coords_uncertainty_meters A numeric value or a vector of numeric
#' values representing the coordinate uncertainty (in meters) associated with
#' each observation. If a single numeric value is provided, it will be applied
#' to all observations. If a numeric vector is provided, it must be the same
#' length as the number of observations.
#'
#' @returns The input data frame or an sf object with POINT geometry, with an
#' additional column named `coordinateUncertaintyInMeters` that contains the
#' coordinate uncertainty values in meters.
#'
#' @export
#'
#' @import sf
#' @importFrom stats setNames
#'
#' @family main
#'
#' @examples
#' # Create dataframe with sampling status column
#' observations_data <- data.frame(
#'     time_point = 1,
#'     sampling_prob = seq(0.5, 1, 0.1)
#'   )
#'
#' # provide a fixed uncertainty for all points
#' add_coordinate_uncertainty(
#'   observations_data,
#'   coords_uncertainty_meters = 1000
#'  )
#'
#' # add variability in uncertainty. For example, using gamma distribution
#' uncertainty_vec <- seq(50, 100, 10)
#'
#' add_coordinate_uncertainty(
#'   observations_data,
#'   coords_uncertainty_meters = uncertainty_vec
#' )

add_coordinate_uncertainty <- function(
    observations,
    coords_uncertainty_meters = 25) {
  ### Start checks
  # 1. Check input type and length
  # Check if observations is an sf object
  stopifnot("`observations` must be an sf object or a dataframe." =
              inherits(observations, "sf") ||
              inherits(observations, "data.frame"))

  # Check if coords_uncertainty_meters is numeric
  stopifnot("`coords_uncertainty_meters` must be  numeric vector." =
              is.numeric(coords_uncertainty_meters))

  # 2. Other checks
  # Number of observations and values in coords_uncertainty_meters must be the
  # same when number of values is larger than 1
  if (length(coords_uncertainty_meters) > 1) {
    size_match <- length(coords_uncertainty_meters) == nrow(observations)
    error_message <- paste(
      "Number of values in `coords_uncertainty_meters` differs from the number",
      "of observations."
    )
    do.call(stopifnot, stats::setNames(list(size_match), error_message))
  }
  ### End checks

  observations$coordinateUncertaintyInMeters <- coords_uncertainty_meters

  return(observations)
}
