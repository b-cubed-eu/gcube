## Prepare example datasets
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

# Convert the polygon to an sf object
plgn_sf <- st_sfc(plgn)

# Assign a CRS to the sf object
example_crs <- 4326
plgn_sf <- st_sf(geometry = plgn_sf, crs = example_crs)

## Unit Tests
test_that("simulate_occurrences returns an sf object with POINT geometry", {
  result <- simulate_occurrences(plgn,
                                 initial_average_occurrences = 50,
                                 n_time_points = 1)

  expect_s3_class(result, "sf")
  expect_true(all(st_geometry_type(result) == "POINT"))
  expect_true(length(unique(result$time_point)) == 1)
})

test_that("simulate_occurrences returns reproducible results with a seed", {
  seed <- 123
  result1 <- simulate_occurrences(plgn,
                                  initial_average_occurrences = 50,
                                  n_time_points = 1,
                                  seed = seed)
  result2 <- simulate_occurrences(plgn,
                                  initial_average_occurrences = 50,
                                  n_time_points = 1,
                                  seed = seed)

  expect_equal(result1, result2)
})

test_that("simulate_occurrences handles different spatial_pattern values", {
  result_random <- simulate_occurrences(plgn,
                                        initial_average_occurrences = 50,
                                        spatial_pattern = "random",
                                        n_time_points = 1)
  result_clustered <- simulate_occurrences(plgn,
                                           initial_average_occurrences = 50,
                                           spatial_pattern = "clustered",
                                           n_time_points = 1)

  expect_s3_class(result_random, "sf")
  expect_s3_class(result_clustered, "sf")
  expect_true(all(st_geometry_type(result_random) == "POINT"))
  expect_true(all(st_geometry_type(result_clustered) == "POINT"))
  expect_true(length(unique(result_random$time_point)) == 1)
  expect_true(length(unique(result_clustered$time_point)) == 1)
})

test_that("simulate_occurrences handles different n_time_points values", {
  n_time_points <- 4
  result <- simulate_occurrences(plgn,
                                 initial_average_occurrences = 50,
                                 temporal_function = simulate_random_walk,
                                 n_time_points = n_time_points)

  expect_s3_class(result, "sf")
  expect_true(all(st_geometry_type(result) == "POINT"))
  expect_equal(length(unique(result$time_point)), n_time_points)
  expect_true(length(unique(result$time_point)) == n_time_points)
})

test_that("simulate_occurrences raises an error for incorrect plgn type", {
  plgn <- list() # Not an sf object
  expect_error(simulate_occurrences(plgn),
               "`species_range` must be an sf object with POLYGON geometry.")
})

test_that("simulate_occurrences raises an error for non-numeric abundance", {
  expect_error(
    simulate_occurrences(
      plgn,
      initial_average_occurrences = "not_numeric"
    ),
    "`initial_average_occurrences` must be a single positive number."
  )
})

test_that("simulate_occurrences raises an error for invalid spatial_pattern", {
  expect_error(
    simulate_occurrences(plgn, spatial_pattern = "invalid_value"),
    paste("`spatial_pattern` must be one of 'random', 'clustered', or a",
          "single number larger or equal to 1.")
  )
})

test_that("simulate_occurrences raises an error for non-integer time points", {
  expect_error(
    simulate_occurrences(plgn, n_time_points = "not_integer"),
    "`n_time_points` must be a single positive integer."
  )
})

test_that("simulate_occurrences raises an error for non-function", {
  expect_error(simulate_occurrences(plgn, temporal_function = "not_a_function"),
               "`temporal_function` must be `NA` or a function.")
})

test_that("simulate_occurrences raises an error for invalid seed type", {
  expect_error(simulate_occurrences(plgn, seed = "not_numeric"),
               "`seed` must be a numeric vector of length 1 or NA.")
})

test_that("simulate_occurrences returns different results without a seed", {
  result1 <- simulate_occurrences(plgn,
                                  initial_average_occurrences = 50,
                                  n_time_points = 1)
  result2 <- simulate_occurrences(plgn,
                                  initial_average_occurrences = 50,
                                  n_time_points = 1)

  expect_false(identical(result1, result2))
})

test_that("simulate_occurrences handles CRS correctly", {
  result <- simulate_occurrences(plgn_sf,
                                 initial_average_occurrences = 50,
                                 temporal_function = simulate_random_walk,
                                 n_time_points = 5)

  expect_equal(sf::st_crs(plgn_sf)$wkt, sf::st_crs(result)$wkt)
})
