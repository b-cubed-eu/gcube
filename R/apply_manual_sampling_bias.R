#' Generate a sampling bias via a grid
#'
#' The function adds a sampling bias weight column containing the sample
#' probability based on bias weights within each cell of a given grid layer.
#'
#' @param occurrences_sf An sf object with POINT geometry.
#' @param bias_weights A raster layer (sf object with POLYGON geometry). The
#' raster of bias weights to be applied to the sampling of occurrences. This sf
#' object should contain a `bias_weight` and `geometry` column. Higher weights
#' indicate a higher probability of sampling. Weights must be numeric values
#' between 0 and 1 OR positive integers that will be rescaled to values between
#' 0 and 1.
#'
#' @returns An sf object with POINT geometry with a bias_weight column
#' containing the sampling probability based on sampling bias.
#'
#' @export
#'
#' @import sf
#' @importFrom stats setNames
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
#' )
#'
#' # Convert the occurrence data to an sf object
#' occurrences_sf <- st_as_sf(occurrences, coords = c("lon", "lat"))
#'
#' # Create raster grid
#' grid <- st_make_grid(occurrences_sf) %>%
#'   st_sf()
#'
#' # Bias weights between 0 and 1
#' grid1 <- grid %>%
#'   mutate(bias_weight = runif(nrow(grid), min = 0, max = 1))
#'
#' apply_manual_sampling_bias(occurrences_sf, grid1)
#'
#' # Bias weights larger than 1
#' grid2 <- grid %>%
#'   mutate(bias_weight = rpois(nrow(grid), 5))
#'
#' occurrence_bias_sf <- apply_manual_sampling_bias(occurrences_sf, grid2)
#' occurrence_bias_sf
#'
#' # Visualise where the bias is
#' ggplot() +
#'  geom_sf(data = grid2) +
#'  geom_sf_text(data = grid2, aes(label = bias_weight)) +
#'  geom_sf(data = occurrence_bias_sf, aes(colour = bias_weight)) +
#'  scale_color_gradient(trans = "reverse")

apply_manual_sampling_bias <- function(occurrences_sf, bias_weights) {
  ### Start checks
  # 1. Check input type and length
  # Check if occurrences_sf is an sf object with point geometry
  stopifnot("`occurrences_sf` must be an sf object." =
              inherits(occurrences_sf, "sf") &&
              sf::st_geometry_type(observations,
                                   by_geometry = FALSE) == "POINT")

  # Check if bias_weights is an sf object with POLYGON geometry
  stopifnot("`bias_weights` must be an sf object." =
              inherits(bias_weights, "sf") &&
              sf::st_geometry_type(bias_weights,
                                   by_geometry = FALSE) == "POLYGON")


  # 2. Other checks
  # Check if bias_weights has a column named bias_weight
  stopifnot("`bias_weights` must have a column named `bias_weight`." =
              "bias_weight" %in% colnames(bias_weights))

  # Check if the values of bias_weights$bias_weight are positive values
  error_message <- paste("The column `bias_weight` must consist of numeric",
                         "values between 0 and 1, or positive integers.")
  do.call(stopifnot,
          stats::setNames(
            list(is.numeric(bias_weights$bias_weight),
                 all(bias_weights$bias_weight >= 0)
                 ),
                 rep(error_message, 2)
            )
          )

  # Check if the values of bias_weights$bias_weight are positive integers
  if (max(bias_weights$bias_weight) > 1) {
    do.call(stopifnot,
            stats::setNames(
              list(all(bias_weights$bias_weight %% 1 == 0)), error_message
              )
            )
  }

  # CRS of sf objects
  stopifnot("`bias_weights` must have the same CRS as `observations`." =
              sf::st_crs(observations) == sf::st_crs(bias_weights))

  # Check if all occurrences (points) are in the grid
  points_in_grid <- sf::st_filter(occurrences_sf, bias_weights)
  stopifnot("`bias_weights` must be a grid that encompasses all occurrences." =
              identical(points_in_grid, occurrences_sf))
  ### End checks

  # Rescale bias_weight if needed
  maxweight <- max(bias_weights$bias_weight)
  if (maxweight > 1) {
    bias_weights$bias_weight <- bias_weights$bias_weight / maxweight
  }

  # Explicitly assume that the attribute is constant throughout the geometry
  sf::st_agr(occurrences_sf) <- "constant"
  sf::st_agr(bias_weights) <- "constant"

  # Take intersection to add bias weights to occurrence points
  weighted_occurrences <- sf::st_intersection(occurrences_sf, bias_weights)

  return(weighted_occurrences)
}
