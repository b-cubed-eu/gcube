#' Add coordinate uncertainty to observations
#'
#' Adds a column to the observations sf object with the coordinate uncertainty
#' in meters.
#'
#'
#' @param observations An sf object with POINT geometry.
#' @param coords_uncertainty_meters A numeric value or a vector of numeric
#' values indicating the coordinate uncertainty associated with observations.
#'
#' @return An sf object with POINT geometry with an additional column
#' `coordinateUncertaintyInMeters`.
#'
#' @export
#'
#' @import sf
#' @importFrom stats setNames
#'
#' @family main
#'
#' @examples
#'
#' library(sf)
#' library(dplyr)
#'
#' set.seed(123)
#'
#' # Create four random points
#' n_points <- 4
#' xlim <- c(3841000, 3842000)
#' ylim <- c(3110000, 3112000)
#' observations_sf <- data.frame(
#'   lat = runif(n_points, ylim[1], ylim[2]),
#'     long = runif(n_points, xlim[1], xlim[2])) %>%
#'     st_as_sf(coords = c("long", "lat"), crs = 3035)
#'
#'  # provide a fixed uncertainty for all points
#'  add_coordinate_uncertainty(
#'    observations_sf,
#'    coords_uncertainty_meters = 1000
#'    )
#'
#' # add variability in uncertainty. For example, using gamma distribution
#' add_coordinate_uncertainty(
#'   observations_sf,
#'   coords_uncertainty_meters = rgamma(n_points, shape = 5, rate = 0.1)
#' )

add_coordinate_uncertainty <- function(
    observations,
    coords_uncertainty_meters = 25) {
  ### Start checks
  # 1. Check input type and length
  # Check if observations is an sf object
  stopifnot("`observations` must be an sf object with POINT geometry." =
              inherits(observations, "sf") &&
              sf::st_geometry_type(observations,
                                   by_geometry = FALSE) == "POINT")

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
