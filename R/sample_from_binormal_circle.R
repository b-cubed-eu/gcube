#' Sample from a circle using the bivariate Normal distribution
#'
#' This function samples a new observations point of a species within the
#' uncertainty circle around each observation assuming a bivariate Normal
#' distribution.
#'
#' @param observations An sf object with POINT geometry and a `time_point` and
#' `coordinateUncertaintyInMeters` column. If the former column is not present,
#' the function will assume a single time point. If the latter column is not
#' present, the function will assume no uncertainty (zero meters) around the
#' observation points.
#' @param p_norm A numeric value between 0 and 1. The proportion of all possible
#' samples from a bivariate Normal distribution that fall within the uncertainty
#' circle. Default is 0.95.
#' See Details.
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
#' @details A new observation point is sampled from a bivariate Normal
#' distribution with means equal to the X and Y coordinates of its original
#' observation point and variances equal to
#' (-`coordinateUncertaintyInMeters`^2) / (2 * log(1 - `p_norm`)),
#' ensuring `p_norm` % of all possible samples fall within the uncertainty
#' circle.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' sampled occurrences and a `coordinateUncertaintyInMeters` column containing
#' the coordinate uncertainty for each observation.
#'
#' @export
#'
#' @import dplyr
#' @import sf
#' @import assertthat
#' @importFrom rlang .data
#' @importFrom mnormt rmnorm
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
#' # Sample points within uncertainty circles according to normal rules
#' sample_from_binormal_circle(
#'   observations = observations_sf,
#'   p_norm = 0.95,
#'   seed = 123
#' )

sample_from_binormal_circle <- function(
    observations,
    p_norm = 0.95,
    missing_uncertainty = 1000,
    seed = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if observations is an sf object
  stopifnot("`observations` must be an sf object." =
              inherits(observations, "sf"))

  # p_norm should be numeric between 0 and 1
  stopifnot("`pnorm` must be a numeric value between 0 and 1." =
              assertthat::is.number(p_norm) &
              (p_norm > 0 & p_norm < 1))

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
      on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv))
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

    # New points are equal to original points in case of no uncertainty
    new_points <- observations %>%
      dplyr::select("time_point", "coordinateUncertaintyInMeters")
  } else {
    # Fill in potential missing coordinate uncertainty
    observations$coordinateUncertaintyInMeters <-
      dplyr::coalesce(observations$coordinateUncertaintyInMeters,
                      missing_uncertainty)

    # Calculate 2-dimensional means and variance-covariance matrices
    means <- sf::st_coordinates(observations$geometry)
    variances <- (-observations$coordinateUncertaintyInMeters^2) /
      (2 * log(1 - p_norm))
    varcovariances <- lapply(variances, function(var) {
      matrix(c(var, -1, -1, var), nrow = 2)
    })

    # Sample new points from bivariate Normal distribution
    new_points_list <- vector("list", length = nrow(observations))
    for (i in seq_len(nrow(observations))) {
      new_points_list[[i]] <- mnormt::rmnorm(
        1,
        mean = means[i, ], varcov = varcovariances[[i]]
      )
    }
    new_points_df <- do.call(rbind.data.frame, new_points_list)
    names(new_points_df) <- c("x_new", "y_new")

    # Create geometry and add uncertainties
    new_points <- cbind(
      new_points_df,
      time_point = observations$time_point,
      coordinateUncertaintyInMeters = observations$coordinateUncertaintyInMeters
    ) %>%
      sf::st_as_sf(coords = c("x_new", "y_new"), crs = sf::st_crs(observations))
  }

  return(new_points)
}
