#' Simulate a random walk over time
#'
#' This function simulates a timeseries for the average number of occurrences of
#' a species using a random walk over time.
#'
#' @param initial_average_occurrences A positive numeric value indicating the
#' average number of occurrences to be simulated at the first time point.
#' @param n_time_points A positive integer specifying the number of time points
#' to simulate.
#' @param sd_step A positive numeric value indicating the standard deviation of
#' the random steps.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), then `set.seed()`
#' is not called at all. If not `NA`, then the random number generator state is
#' reset (to the state before calling this function) upon exiting this function.
#'
#' @returns A vector of integers of length `n_time_points` with the average
#' number of occurrences.
#'
#' @export
#'
#' @import assertthat
#' @importFrom stats rnorm
#'
#' @family occurrence
#'
#' @examples
#' simulate_random_walk(
#'   initial_average_occurrences = 50,
#'   n_time_points = 10,
#'   sd_step = 1,
#'   seed = 123
#' )

simulate_random_walk <- function(
    initial_average_occurrences = 50,
    n_time_points = 10,
    sd_step = 0.05,
    seed = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if initial_average_occurrences is a positive number
  stopifnot("`initial_average_occurrences` must be a single positive number." =
              assertthat::is.number(initial_average_occurrences) &
              initial_average_occurrences >= 0)

  # Check if n_time_points is a positive integer
  stopifnot("`n_time_points` must be a single positive integer." =
              assertthat::is.count(n_time_points))

  # Check if sd_step is a positive number
  stopifnot("`sd_step` must be a single positive number." =
              assertthat::is.number(sd_step) & sd_step >= 0)

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (assertthat::is.number(seed) | is.na(seed)) &
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

  # Initialize an empty vector to store average abundance values
  lambdas <- numeric(n_time_points)

  # Set the initial abundance
  lambdas[1] <- initial_average_occurrences

  # Generate random steps and accumulate them
  for (i in 2:n_time_points) {
    step <- stats::rnorm(1, mean = 0, sd = sd_step)
    lambdas[i] <- lambdas[i - 1] + step
  }

  # Identify where the lambda values become 0 or lower
  zero_or_lower_index <- which(lambdas <= 0)

  # If any lambda becomes 0 or lower, set all subsequent lambdas to 0
  if (length(zero_or_lower_index) > 0) {
    zero_or_lower_indices <- zero_or_lower_index[1]:n_time_points
    lambdas[zero_or_lower_indices] <- 0
  }

  # Return samples from Poisson
  return(lambdas)
}
