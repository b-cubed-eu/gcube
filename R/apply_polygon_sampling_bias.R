#' Generate a sampling bias via a polygon
#'
#' The function adds a sampling bias weight column containing the sample
#' probability based on bias strength within a given polygon.
#'
#' @param occurrences_sf An sf object with POINT geometry.
#' @param bias_area An sf object with POLYGON geometry. The area in which the
#' sampling will be biased.
#' @param bias_strength A positive numeric value. The strength of the bias to
#' be applied in the biased area (as a multiplier). Above 1, area will be
#' oversampled. Below 1, area will be undersampled. For example, a value of 50
#' will result in 50 times more samples within the bias_area than outside.
#' Conversely, a value of 0.5 will result in half less samples within the
#' bias_area than outside.
#'
#' @returns An sf object with POINT geometry with a bias_weight column
#' containing the sampling probability based on sampling bias.
#'
#' @export
#'
#' @import sf
#' @import dplyr
#' @import assertthat
#'
#' @family detection
#'
#' @examples
#' # Load packages
#' library(sf)
#' library(dplyr)
#' library(ggplot2)
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
#' # Create bias_area polygon overlapping at least two of the points
#' selected_observations <- st_union(occurrences_sf[2:3,])
#' bias_area <- st_convex_hull(selected_observations) %>%
#'   st_buffer(dist = 100) %>%
#'   st_as_sf()
#'
#' occurrence_bias_sf <- apply_polygon_sampling_bias(
#'   occurrences_sf,
#'   bias_area,
#'   bias_strength = 2)
#' occurrence_bias_sf
#'
#' # Visualise where the bias is
#' occurrence_bias_sf %>%
#'   mutate(bias_weight_f = as.factor(round(bias_weight, 3))) %>%
#'   ggplot() +
#'     geom_sf(data = bias_area) +
#'     geom_sf(aes(colour = bias_weight_f)) +
#'     ggtitle("Sampling Bias via Polygon")

apply_polygon_sampling_bias <- function(
    occurrences_sf,
    bias_area,
    bias_strength = 1) {
  ### Start checks
  # 1. Check input type and length
  # Check if occurrences_sf is an sf object with point geometry
  stopifnot("`occurrences_sf` must be an sf object." =
              inherits(occurrences_sf, "sf") &&
              sf::st_geometry_type(occurrences_sf,
                                   by_geometry = FALSE) == "POINT")

  # Check if bias_area is an sf object with POLYGON geometry
  stopifnot("`bias_area` must be an sf object." =
              inherits(bias_area, "sf") &&
              sf::st_geometry_type(bias_area,
                                   by_geometry = FALSE) == "POLYGON")

  # Check if bias_strength is a positive number
  stopifnot("`bias_strength` must be a single positive number." =
              assertthat::is.number(bias_strength) & bias_strength >= 0)
  ### End checks

  # Combine polygons into multipolygon
  bias_area <- sf::st_union(bias_area)

  # Find occurrences inside polygon
  in_bias_area <- occurrences_sf %>%
    sf::st_within(bias_area, sparse = FALSE) %>%
    as.vector()

  # Calculate sampling probability based on bias strength
  bias_weights_outside_polygon <- 1 / (1 + bias_strength)
  bias_weights_inside_polygon <- bias_strength / (1 + bias_strength)

  #create bias_weight column
  occurrences_sf <- occurrences_sf %>%
    dplyr::mutate(bias_weight = ifelse(in_bias_area,
                                       bias_weights_inside_polygon,
                                       bias_weights_outside_polygon))

  return(occurrences_sf)
}
