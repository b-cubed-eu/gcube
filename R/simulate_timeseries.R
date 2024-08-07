#' Simulate timeseries for species abundances
#'
#' The function simulates a timeseries for the abundance of a species.
#'
#' @param initial_average_occurrences A positive numeric value indicating the
#' average number of occurrences to be simulated within the extend of `polygon`
#' at the first time point. This value will be used as mean of a Poisson
#' distribution (lambda parameter).
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param temporal_function `NA` (default), or a function which generates
#' a trend in abundance over time. Only used if `n_time_points > 1`. By default,
#' the function will sample `n_time_points` times from a Poisson
#' distribution with average (lambda) `initial_average_occurrences`. When a
#' function is specified (e.g. the internal `simulate_random_walk()` function)
#' `n_time_points` average abundances (lambdas) are calculated using
#' `initial_average_occurrences` and any additional arguments passed.
#' See examples.
#' @param ... Additional argument to be passed to the `temporal_function`
#' function.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @returns A vector of integers of length n_time_points with the number of
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
#' library(ggplot2)
#'
#' ## 1. Use the function simulate_random_walk()
#' simulate_timeseries(
#'   initial_average_occurrences = 50,
#'   n_time_points = 10,
#'   temporal_function = simulate_random_walk,
#'   sd_step = 1,
#'   seed = 123
#' )
#'
#' ## 2. Visualising multiple draws
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Draw n_sim abundances from Poisson distribution using random walk
#' n_sim <- 10
#' n_time_points <- 50
#' sd_step <- 1
#' list_abundances <- vector("list", length = n_sim)
#'
#' # Loop n_sim times over simulate_timeseries()
#' for (i in seq_len(n_sim)) {
#'   abundances <- simulate_timeseries(
#'     initial_average_occurrences = 50,
#'     n_time_points = n_time_points,
#'     temporal_function = simulate_random_walk,
#'     sd_step = sd_step
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
#' ggplot(data_abundances, aes(x = time, y = abundance, colour = factor(sim))) +
#'   geom_line() +
#'   labs(
#'     x = "Time", y = "Species abundance",
#'     title = paste(
#'       n_sim, "simulated abundances using random walk",
#'       "with sd =", sd_step
#'     )
#'   ) +
#'   scale_y_continuous(limits = c(0, NA)) +
#'   scale_x_continuous(breaks = seq(0, n_time_points, 5)) +
#'   theme_minimal() +
#'   theme(legend.position = "")
#'
#' ## 3. Using your own function
#' # You can also specify your own trend function, e.g. this linear function
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
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Draw n_sim abundances from Poisson distribution using our own function
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
#' ggplot(data_abundances, aes(x = time, y = abundance, colour = factor(sim))) +
#'   geom_line() +
#'   labs(
#'     x = "Time", y = "Species abundance",
#'     title = paste(
#'       n_sim, "simulated abundances using our own linear function",
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
  stopifnot(
    "`initial_average_occurrences` must be a single positive number." =
      assertthat::is.number(initial_average_occurrences) &
      initial_average_occurrences >= 0)

  # Check if n_time_points is a positive integer
  stopifnot(
    "`n_time_points` must be a single positive integer." =
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
    withr::local_seed(seed)
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
        initial_average_occurrences,
        n_time_points,
        seed = seed
      )
    } else {
      # Generate timeseries using the provided function
      lambdas <- temporal_function(
        initial_average_occurrences,
        n_time_points,
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
