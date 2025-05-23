## Prepare example datasets
# Define a helper function to create a sample raster
create_sample_raster <- function() {
  # Create a SpatRaster object
  r <- terra::rast(nrows = 10, ncols = 10,
                   xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  terra::values(r) <- runif(terra::ncell(r))
  return(r)
}

## Unit Tests
test_that("sample_occurrences_from_raster returns an sf object", {
  raster <- create_sample_raster()
  time_series <- 10:20
  result <- sample_occurrences_from_raster(raster, time_series)

  expect_s3_class(result, "sf")
  expect_true(all(st_geometry_type(result) == "POINT"))
  expect_equal(nrow(result), sum(time_series))
})

test_that("sample_occurrences_from_raster returns reproducible results", {
  raster <- create_sample_raster()
  time_series <- 10:20
  seed <- 123

  result1 <- sample_occurrences_from_raster(raster, time_series, seed)
  result2 <- sample_occurrences_from_raster(raster, time_series, seed)

  expect_equal(result1, result2)
})

test_that("sample_occurrences_from_raster raises error for raster", {
  raster <- list() # Not a SpatRaster object
  time_series <- 10

  expect_error(sample_occurrences_from_raster(raster, time_series),
               "`raster` must be a SpatRaster object.")
})

test_that("sample_occurrences_from_raster raises error time_series", {
  raster <- create_sample_raster()
  time_series <- "not_numeric"

  expect_error(sample_occurrences_from_raster(raster, time_series),
               "`time_series` must be a positive numeric vector.")
})

test_that("sample_occurrences_from_raster raises error for neg. time_series", {
  raster <- create_sample_raster()
  time_series <- -10

  expect_error(sample_occurrences_from_raster(raster, time_series),
               "`time_series` must be a positive numeric vector.")
})

test_that("sample_occurrences_from_raster raises an error for incorrect seed", {
  raster <- create_sample_raster()
  time_series <- 10
  seed <- "not_numeric"

  expect_error(sample_occurrences_from_raster(raster, time_series, seed),
               "`seed` must be a numeric vector of length 1 or NA.")
})

test_that("sample_occurrences_from_raster returns diff. results without seed", {
  raster <- create_sample_raster()
  time_series <- 10

  result1 <- sample_occurrences_from_raster(raster, time_series)
  result2 <- sample_occurrences_from_raster(raster, time_series)

  expect_false(identical(result1, result2))
})

test_that("sample_occurrences_from_raster handles CRS correctly", {
  raster <- create_sample_raster()
  time_series <- 10:20
  result <- sample_occurrences_from_raster(raster, time_series)

  expect_equal(terra::crs(raster), sf::st_crs(result)$wkt)
})
