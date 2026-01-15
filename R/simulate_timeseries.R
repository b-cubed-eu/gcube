#' Simulate timeseries for species occurrences
#'
#' This function simulates a timeseries for the number of occurrences of a
#' species.
#'
#' @param initial_average_occurrences A positive numeric value indicating the
#' average number of occurrences to be simulated at the first time point. This
#' value serves as the mean (lambda) of a Poisson distribution.
#' @param n_time_points A positive integer specifying the number of time points
#' to simulate.
#' @param temporal_function A function generating a trend in number of
#' occurrences over time, or `NA` (default). If `n_time_points` > 1 and a
#' function is provided, it defines the temporal pattern of number of
#' occurrences.
#' @param ... Additional arguments to be passed to `temporal_function`.
#' @param seed A positive numeric value setting the seed for random number
#' generation to ensure reproducibility. If `NA` (default), then `set.seed()`
#' is not called at all. If not `NA`, then the random number generator state is
#' reset (to the state before calling this function) upon exiting this function.
#'
#' @returns A vector of integers of length `n_time_points` with the number of
#' occurrences.
#'
#' @export
#'
#' @import assertthat
#' @importFrom stats rpois
#'
#' @family occurrence
#'
#' @examples
#' # 1. Use the function simulate_random_walk()
#' simulate_timeseries(
#'   initial_average_occurrences = 50,
#'   n_time_points = 10,
#'   temporal_function = simulate_random_walk,
#'   sd_step = 1,
#'   seed = 123
#' )
#'
#' # 2. Using your own custom function, e.g. this linear function
#' my_own_linear_function <- function(
#'     initial_average_occurrences = initial_average_occurrences,
#'     n_time_points = n_time_points,
#'     coef) {
#'   # Calculate new average abundances over time
#'   time <- seq_len(n_time_points) - 1
#'   lambdas <- initial_average_occurrences + (coef * time)
#'
#'   # Identify where the lambda values become 0 or lower
#'   zero_or_lower_index <- which(lambdas <= 0)
#'
#'   # If any lambda becomes 0 or lower, set all subsequent lambdas to 0
#'   if (length(zero_or_lower_index) > 0) {
#'     zero_or_lower_indices <- zero_or_lower_index[1]:n_time_points
#'     lambdas[zero_or_lower_indices] <- 0
#'   }
#'
#'   # Return average abundances
#'   return(lambdas)
#' }
#'
#' # Draw n_sim number of occurrences from Poisson distribution using
#' # the custom function
#' n_sim <- 10
#' n_time_points <- 50
#' slope <- 1
#' list_abundances <- vector("list", length = n_sim)
#'
#' # Loop n_sim times over simulate_timeseries()
#' for (i in seq_len(n_sim)) {
#'   abundances <- simulate_timeseries(
#'     initial_average_occurrences = 50,
#'     n_time_points = n_time_points,
#'     temporal_function = my_own_linear_function,
#'     coef = slope
#'   )
#'
#'   list_abundances[[i]] <- data.frame(
#'     time = seq_along(abundances),
#'     abundance = abundances,
#'     sim = i
#'   )
#' }
#'
#' # Combine list of dataframes
#' data_abundances <- do.call(rbind.data.frame, list_abundances)
#'
#' # Plot the simulated abundances over time using ggplot2
#' library(ggplot2)
#' ggplot(data_abundances, aes(x = time, y = abundance, colour = factor(sim))) +
#'   geom_line() +
#'   labs(
#'     x = "Time", y = "Species abundance",
#'     title = paste(
#'       n_sim, "simulated trends using custom linear function",
#'       "with slope", slope
#'     )
#'   ) +
#'   scale_y_continuous(limits = c(0, NA)) +
#'   scale_x_continuous(breaks = seq(0, n_time_points, 5)) +
#'   theme_minimal() +
#'   theme(legend.position = "")

simulate_timeseries <- function(
    initial_average_occurrences = 50,
    n_time_points = 1,
    temporal_function = NA,
    ...,
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

  # Check if temporal_function is NA or a function
  stopifnot("`temporal_function` must be `NA` or a function." =
              (is.function(temporal_function) || is.na(temporal_function)) &
              length(temporal_function) == 1)

  # Check if seed is NA or a number
  stopifnot("`seed` must be a numeric vector of length 1 or NA." =
              (assertthat::is.number(seed) | is.na(seed)) &
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

  # Check type of temporal_function
  # If temporal_function is a function, use it to generate the timeseries
  if (is.function(temporal_function) && n_time_points > 1) {
    # Collect additional arguments
    length_pars <- length(list(...))

    # If arguments are empty, pass nothing to the function
    if (length_pars == 0) {
      # Generate timeseries using the provided function
      lambdas <- temporal_function(
        initial_average_occurrences = initial_average_occurrences,
        n_time_points = n_time_points
      )
    } else {
      # Generate timeseries using the provided function
      lambdas <- temporal_function(
        initial_average_occurrences = initial_average_occurrences,
        n_time_points = n_time_points,
        ...
      )
    }
    timeseries <- stats::rpois(n_time_points, lambdas)
  } else {
    # When it's NA, generate timeseries using a Poisson distribution
    timeseries <- stats::rpois(n_time_points, initial_average_occurrences)
  }

  return(timeseries)
}
