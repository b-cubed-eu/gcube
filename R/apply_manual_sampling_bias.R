#' Apply manual sampling bias to occurrences via a grid
#'
#' This function adds a sampling bias weight column to an sf object containing
#' occurrences. The sampling probabilities are based on bias weights within each
#' cell of a provided grid layer.
#'
#' @param occurrences_sf An sf object with POINT geometry representing the
#' occurrences.
#' @param bias_weights An `sf` object with POLYGON geometry representing the
#' grid with bias weights. This sf object should contain a `bias_weight` column
#' and a `geometry` column. Higher weights indicate a higher probability of
#' sampling. Weights must be numeric values between 0 and 1 or positive
#' integers, which will be rescaled to values between 0 and 1.
#'
#' @returns An sf object with POINT geometry that includes a `bias_weight`
#' column containing the sampling probabilities based on the sampling bias.
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
#' # Create polygon
#' plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))
#'
#' # Get occurrence points
#' occurrences_sf <- simulate_occurrences(plgn)
#'
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Create grid with bias weights
#' grid <- st_make_grid(
#'     plgn,
#'     n = c(10, 10),
#'     square = TRUE) %>%
#'   st_sf()
#' grid$bias_weight <- runif(nrow(grid), min = 0, max = 1)
#'
#' # Calculate occurrence bias
#' occurrence_bias <- apply_manual_sampling_bias(occurrences_sf, grid)
#' occurrence_bias
#'
#' # Visualise where the bias is
#' ggplot() +
#'   geom_sf(data = plgn) +
#'   geom_sf(data = grid, alpha = 0) +
#'   geom_sf(data = occurrence_bias, aes(colour = bias_weight)) +
#'   geom_sf_text(data = grid, aes(label = round(bias_weight, 2))) +
#'   theme_minimal()

apply_manual_sampling_bias <- function(occurrences_sf, bias_weights) {
  ### Start checks
  # 1. Check input type and length
  # Check if occurrences_sf is an sf object with point geometry
  stopifnot("`occurrences_sf` must be an sf object." =
              inherits(occurrences_sf, "sf") &&
              sf::st_geometry_type(occurrences_sf,
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
  stopifnot("`bias_weights` must have the same CRS as `occurrences_sf`." =
              sf::st_crs(occurrences_sf) == sf::st_crs(bias_weights))

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
