## Sample data for testing
num_points <- 10
occurrences <- data.frame(
  lon = runif(num_points, min = -180, max = 180),
  lat = runif(num_points, min = -90, max = 90),
  time_point = 0
)
occurrences_sf <- st_as_sf(occurrences, coords = c("lon", "lat"))

# Create bias_area polygon
selected_observations <- st_union(occurrences_sf[2:3, ])
bias_area <- st_convex_hull(selected_observations) %>%
  st_buffer(dist = 100) %>%
  st_as_sf()

# Create raster grid with bias weights
grid <- st_make_grid(occurrences_sf) %>%
  st_sf() %>%
  mutate(bias_weight = runif(n(), min = 0, max = 1))


## Unit Tests
test_that("Function samples observations without sampling bias correctly", {
  result <- sample_observations(
    occurrences_sf,
    detection_probability = 0.8,
    sampling_bias = "no_bias",
    seed = 123
  )
  expect_true("sampling_status" %in% colnames(result))
  expect_equal(nrow(result), num_points)
})

test_that("Function samples observations with polygon sampling bias", {
  result <- sample_observations(
    occurrences_sf,
    detection_probability = 0.8,
    sampling_bias = "polygon",
    bias_area = bias_area,
    bias_strength = 2,
    seed = 123
  )
  expect_true("sampling_status" %in% colnames(result))
  expect_equal(nrow(result), num_points)
})

test_that("Function samples observations with manual sampling bias correctly", {
  result <- sample_observations(
    occurrences_sf,
    detection_probability = 0.8,
    sampling_bias = "manual",
    bias_weights = grid,
    seed = 123
  )
  expect_true("sampling_status" %in% colnames(result))
  expect_equal(nrow(result), num_points)
})

test_that("Function throws an error if occurrences is not an sf object", {
  non_sf_data <- data.frame(
    lon = runif(num_points, min = -180, max = 180),
    lat = runif(num_points, min = -90, max = 90)
  )
  expect_error(
    sample_observations(non_sf_data,
                        detection_probability = 0.8,
                        sampling_bias = "no_bias"),
    "`occurrences` must be an sf object.")
})

test_that("Function throws an error if detection_probability is not correct", {
  expect_error(
    sample_observations(occurrences_sf,
                        detection_probability = -0.5,
                        sampling_bias = "no_bias"),
    "`detection_probability` must be a numeric value between 0 and 1.")
  expect_error(
    sample_observations(occurrences_sf,
                        detection_probability = 1.5,
                        sampling_bias = "no_bias"),
    "`detection_probability` must be a numeric value between 0 and 1.")
})

test_that("Function throws an error if seed is not a number or NA", {
  expect_error(
    sample_observations(occurrences_sf,
                        detection_probability = 0.8,
                        sampling_bias = "no_bias",
                        seed = "not_a_number"),
    "`seed` must be a numeric vector of length 1 or NA.")
})

test_that("Function throws an error if sampling_bias is invalid", {
  expect_error(
    sample_observations(occurrences_sf,
                        detection_probability = 0.8,
                        sampling_bias = "invalid_bias"),
    "`sampling_bias` must be one of 'no_bias', 'polygon', 'manual'.")
})

test_that("Function returns the correct columns", {
  result <- sample_observations(
    occurrences_sf,
    detection_probability = 0.8,
    sampling_bias = "no_bias",
    seed = 123
  )
  expect_true(
    all(c("detection_probability",
          "bias_weight",
          "sampling_probability",
          "sampling_status",
          "geometry") %in% colnames(result)))
})

test_that("Function samples observations reproducibly with seed", {
  result1 <- sample_observations(
    occurrences_sf,
    detection_probability = 0.8,
    sampling_bias = "no_bias",
    seed = 123
  )
  result2 <- sample_observations(
    occurrences_sf,
    detection_probability = 0.8,
    sampling_bias = "no_bias",
    seed = 123
  )
  expect_equal(result1, result2)
})

test_that("Function handles polygon sampling bias correctly", {
  result <- sample_observations(
    occurrences_sf,
    detection_probability = 0.8,
    sampling_bias = "polygon",
    bias_area = bias_area,
    bias_strength = 2,
    seed = 123
  )
  expect_true(all(result$sampling_probability >= 0 &
                    result$sampling_probability <= 1))
})

test_that("Function handles manual sampling bias correctly", {
  result <- sample_observations(
    occurrences_sf,
    detection_probability = 0.8,
    sampling_bias = "manual",
    bias_weights = grid,
    seed = 123
  )
  expect_true(all(result$sampling_probability >= 0 &
                    result$sampling_probability <= 1))
})

test_that("Function retains the same CRS in output", {
  result <- sample_observations(
    st_set_crs(occurrences_sf, 3035),
    detection_probability = 0.8,
    sampling_bias = "no_bias",
    seed = 123
  )
  expect_equal(st_crs(result), st_crs(st_set_crs(occurrences_sf, 3035)))
})
