#' Sample from a circle using the Uniform distribution
#'
#' This function samples a new observations point of a species within the
#' uncertainty circle around each observation assuming a Uniform distribution.
#'
#' @param observations An sf object with POINT geometry and a `time_point` and
#' `coordinateUncertaintyInMeters` column. If the former column is not present,
#' the function will assume a single time point. If the latter column is not
#' present, the function will assume no uncertainty (zero meters) around the
#' observation points.
#' @param missing_uncertainty A positive numeric value (default: 1000 m) used to
#' replace missing (`NA`) values in the `coordinateUncertaintyInMeters` column.
#' This ensures that all observations have a defined uncertainty radius for
#' sampling. Only applied when the column is present but contains `NA` values;
#' if the column itself is absent, a value of 0 is assumed instead.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), then `set.seed()`
#' is not called at all. If not `NA`, then the random number generator state is
#' reset (to the state before calling this function) upon exiting this function.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' sampled occurrences and a `coordinateUncertaintyInMeters` column containing
#' the coordinate uncertainty for each observation.
#'
#' @export
#'
#' @import dplyr
#' @import sf
#' @importFrom stats runif
#' @importFrom rlang .data
#'
#' @family designation
#'
#' @examples
#' library(sf)
#' library(dplyr)
#'
#' # Create four random points
#' n_points <- 4
#' xlim <- c(3841000, 3842000)
#' ylim <- c(3110000, 3112000)
#' coordinate_uncertainty <- rgamma(n_points, shape = 5, rate = 0.1)
#'
#' observations_sf <- data.frame(
#'   lat = runif(n_points, ylim[1], ylim[2]),
#'   long = runif(n_points, xlim[1], xlim[2]),
#'   time_point = 1,
#'   coordinateUncertaintyInMeters = coordinate_uncertainty
#' ) %>%
#'   st_as_sf(coords = c("long", "lat"), crs = 3035)
#'
#' # Sample points within uncertainty circles according to uniform rules
#' sample_from_uniform_circle(
#'   observations = observations_sf,
#'   seed = 123
#' )

sample_from_uniform_circle <- function(
    observations,
    missing_uncertainty = 1000,
    seed = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if observations is an sf object
  stopifnot("`observations` must be an sf object." =
              inherits(observations, "sf"))

  # Check if missing_uncertainty is a number
  stopifnot("`missing_uncertainty` must be a numeric vector of length 1." =
              is.numeric(missing_uncertainty) &
              length(missing_uncertainty) == 1)

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (is.numeric(seed) | is.na(seed)) &
              length(seed) == 1)
  ### End checks

  # Set seed if provided
  if (!is.na(seed)) {
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      rng_state_old <- get(".Random.seed", envir = .GlobalEnv)
      on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv)) # nolint: object_name_linter
    }
    set.seed(seed)
  }

  # Create time_point column if column not present in data
  if (!"time_point" %in% names(observations)) {
    observations$time_point <- 1
    warning(paste(
      "No column `time_point` present!",
      "Assuming only a single time point.",
      sep = "\n"
    ))
  }

  # Set uncertainty to zero if column not present in data
  if (!"coordinateUncertaintyInMeters" %in% names(observations)) {
    observations$coordinateUncertaintyInMeters <- 0
    warning(paste(
      "No column `coordinateUncertaintyInMeters` present!",
      "Assuming no uncertainty around observations.",
      sep = "\n"
    ))
  } else {
    # Fill in potential missing coordinate uncertainty
    observations$coordinateUncertaintyInMeters <-
      dplyr::coalesce(observations$coordinateUncertaintyInMeters,
                      missing_uncertainty)
  }

  # Get random angle and radius
  is_degree <- isTRUE(sf::st_crs(observations)$units_gdal == "degree")

  uncertainty_points <-
    observations %>%
    dplyr::mutate(
      random_angle = stats::runif(nrow(observations), 0, 2 * pi),
      random_r = sqrt(stats::runif(nrow(observations), 0, 1)) *
        .data$coordinateUncertaintyInMeters
    )

  # Calculate new point
  new_points <-
    uncertainty_points %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      lat = sf::st_coordinates(.data$geometry)[2],
      lon = sf::st_coordinates(.data$geometry)[1],
      displacement = ifelse(
        is_degree,
        list(meters_to_degrees(.data$random_r, .data$lat)),
        list(list(lat = .data$random_r, lon = .data$random_r))
      ),
      x_new = .data$lon + .data$displacement$lon * cos(.data$random_angle),
      y_new = .data$lat + .data$displacement$lat * sin(.data$random_angle)
    ) %>%
    sf::st_drop_geometry() %>%
    sf::st_as_sf(
      coords = c("x_new", "y_new"),
      crs = sf::st_crs(observations)
    ) %>%
    dplyr::select("time_point", "coordinateUncertaintyInMeters")

  return(new_points)
}
