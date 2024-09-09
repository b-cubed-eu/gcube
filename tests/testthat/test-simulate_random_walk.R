## Unit tests
test_that("simulate_random_walk works with default parameters", {
  result <- simulate_random_walk()
  expect_vector(result, ptype = double(), size = 10)
  expect_true(all(result >= 0))
})

test_that("simulate_random_walk handles non-default parameters", {
  result <- simulate_random_walk(
    initial_average_occurrences = 100,
    n_time_points = 5,
    sd_step = 2)
  expect_vector(result, ptype = double(), size = 5)
  expect_true(all(result >= 0))
})

test_that("simulate_random_walk respects seed for reproducibility", {
  result1 <- simulate_random_walk(seed = 123)
  result2 <- simulate_random_walk(seed = 123)
  expect_equal(result1, result2)
})

test_that("simulate_random_walk handles zero and neg. occurrences properly", {
  result <- simulate_random_walk(
    initial_average_occurrences = 1,
    n_time_points = 10,
    sd_step = 5,
    seed = 123)
  expect_equal(result[1], 1)
  expect_true(all(result >= 0))
})

test_that("simulate_random_walk throws errors for invalid inputs", {
  expect_error(
    simulate_random_walk(initial_average_occurrences = -1),
    "`initial_average_occurrences` must be a single positive number.")
  expect_error(
    simulate_random_walk(n_time_points = 0),
    "`n_time_points` must be a single positive integer.")
  expect_error(
    simulate_random_walk(sd_step = -1),
    "`sd_step` must be a single positive number.")
  expect_error(
    simulate_random_walk(seed = "string"),
    "`seed` must be a numeric vector of length 1 or NA.")
})
