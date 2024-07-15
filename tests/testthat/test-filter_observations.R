## Prepare example datasets
# Create the occurrences dataframe
occurrences <- data.frame(
  lon = c(-76.47209, 103.78985, -32.76831, 137.88627, 158.56822,
          -163.59966, 10.11798, 141.27086, 18.51661, -15.61870),
  lat = c(82.230002, -8.399852, 31.962714, 13.074012, -71.473557,
          71.968495, -45.704208, -82.429284, -30.974271, 81.810657),
  time_point = rep(0, 10)
)

# Convert the occurrence data to an sf object
occurrences_sf <- sf::st_as_sf(occurrences, coords = c("lon", "lat"))

# Sample observations without sampling bias
observations_total_sf <- sample_observations(
  occurrences_sf,
  detection_probability = 0.8,
  sampling_bias = "no_bias",
  seed = 123
  )

observations_total_df <- sf::st_drop_geometry(observations_total_sf)


## Unit tests

test_that("filter_observations works with sf object", {
  # Test case 1: Filter detected observations (default behavior)
  detected_observations <- filter_observations(observations_total_sf)
  expect_true(nrow(observations_total_sf) >= nrow(detected_observations))
  expect_true(all(detected_observations$sampling_status == "detected"))

  # Test case 2: Filter undetected observations (invert = TRUE)
  undetected_observations <- filter_observations(occurrences_sf,
                                                 invert = TRUE)
  expect_true(all(undetected_observations$sampling_status != "detected"))
})

test_that("filter_observations works with dataframe", {
  # Test case 1: Filter detected observations (default behavior)
  detected_observations <- filter_observations(observations_total_df)
  expect_true(nrow(observations_total_df) >= nrow(detected_observations))
  expect_true(all(detected_observations$sampling_status == "detected"))

  # Test case 2: Filter undetected observations (invert = TRUE)
  undetected_observations <- filter_observations(observations_total_df,
                                                 invert = TRUE)
  expect_true(all(undetected_observations$sampling_status != "detected"))
})

test_that("filter_observations handles invalid inputs", {
  # Invalid dataframe input
  expect_error(filter_observations(observations_total = list()),
               "`observations_total` must be an sf object or a dataframe.")

  # Invalid invert argument
  expect_error(filter_observations(observations_total = observations_total_sf,
                                   invert = "TRUE"),
               "`invert` must be a logical vector of length 1.")
})
