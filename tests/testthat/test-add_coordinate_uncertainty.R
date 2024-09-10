## Prepare example datasets
# Create the observations dataframe
n_observations <- 4
observations_sf <-
  data.frame(
    lat = runif(n_observations, 3110000, 3112000),
    long = runif(n_observations, 3841000, 3842000)
  ) %>%
  st_as_sf(coords = c("long", "lat"), crs = 3035)


## Unit tests

test_that("Function adds a column with a single numeric uncertainty value", {
  result <- add_coordinate_uncertainty(observations_sf, 1000)
  expect_equal(result$coordinateUncertaintyInMeters, rep(1000, n_observations))
})

test_that("Function adds a column with a vector of uncertainty values", {
  uncertainty_values <- c(500, 1000, 1500, 2000)
  result <- add_coordinate_uncertainty(observations_sf, uncertainty_values)
  expect_equal(result$coordinateUncertaintyInMeters, uncertainty_values)
})

test_that("Function throws an error if observations is not an sf object", {
  expect_error(add_coordinate_uncertainty("non_sf_data", 1000),
               "`observations` must be an sf object or a dataframe.")
})

test_that("Function throws an error if coord. uncert. is not numeric", {
  expect_error(add_coordinate_uncertainty(observations_sf, "not_numeric"),
               "`coords_uncertainty_meters` must be  numeric vector.")
})

test_that("Function throws an error for wrong length of coord. uncert.", {
  expect_error(
    add_coordinate_uncertainty(observations_sf, c(1000, 2000)),
    paste("Number of values in `coords_uncertainty_meters` differs from the",
          "number of observations.")
  )
})

test_that("CRS of output matches CRS of input", {
  result <- add_coordinate_uncertainty(observations_sf, 1000)
  expect_equal(st_crs(result), st_crs(observations_sf))
})
