## Prepare example function
my_linear_function <- function(
    initial_average_occurrences = initial_average_occurrences,
    n_time_points = n_time_points,
    coef) {
  # Calculate new average abundances over time
  time <- seq_len(n_time_points) - 1
  lambdas <- initial_average_occurrences + (coef * time)
  # Identify where the lambda values become 0 or lower
  zero_or_lower_index <- which(lambdas <= 0)
  # If any lambda becomes 0 or lower, set all subsequent lambdas to 0
  if (length(zero_or_lower_index) > 0) {
    zero_or_lower_indices <- zero_or_lower_index[1]:n_time_points
    lambdas[zero_or_lower_indices] <- 0
  }
  # Return average abundances
  return(lambdas)
}

## Unit Tests
test_that("simulate_timeseries works with default parameters", {
  result <- simulate_timeseries()
  expect_vector(result, ptype = integer(), size = 1)
  expect_true(all(result >= 0))
})

test_that("simulate_timeseries works with simulate_random_walk", {
  result <- simulate_timeseries(
    initial_average_occurrences = 50,
    n_time_points = 10,
    temporal_function = simulate_random_walk,
    sd_step = 1
  )
  expect_vector(result, ptype = integer(), size = 10)
  expect_true(all(result >= 0))
})

test_that("simulate_timeseries works with custom temporal function", {
  result <- simulate_timeseries(
    initial_average_occurrences = 50,
    n_time_points = 10,
    temporal_function = my_linear_function,
    coef = -10
  )
  expect_vector(result, ptype = integer(), size = 10)
  expect_true(all(result >= 0))
})

test_that("simulate_timeseries respects seed for reproducibility", {
  result1 <- simulate_timeseries(
    initial_average_occurrences = 50,
    n_time_points = 10,
    temporal_function = simulate_random_walk,
    sd_step = 1,
    seed = 123
  )
  result2 <- simulate_timeseries(
    initial_average_occurrences = 50,
    n_time_points = 10,
    temporal_function = simulate_random_walk,
    sd_step = 1,
    seed = 123
  )
  expect_equal(result1, result2)
})

test_that("simulate_timeseries throws errors for invalid inputs", {
  expect_error(
    simulate_timeseries(initial_average_occurrences = -1),
    "`initial_average_occurrences` must be a single positive number.")
  expect_error(
    simulate_timeseries(n_time_points = 0),
    "`n_time_points` must be a single positive integer.")
  expect_error(
    simulate_timeseries(temporal_function = "string"),
    "`temporal_function` must be `NA` or a function.")
  expect_error(
    simulate_timeseries(seed = "string"),
    "`seed` must be a numeric vector of length 1 or NA.")
})
