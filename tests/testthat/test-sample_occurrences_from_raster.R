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
  rs <- create_sample_raster()
  ts <- 10:20
  result <- sample_occurrences_from_raster(rs, ts)

  expect_s3_class(result, "sf")
  expect_true(all(st_geometry_type(result) == "POINT"))
  expect_equal(nrow(result), sum(ts))
})

test_that("sample_occurrences_from_raster returns reproducible results", {
  rs <- create_sample_raster()
  ts <- 10:20
  seed <- 123

  result1 <- sample_occurrences_from_raster(rs, ts, seed)
  result2 <- sample_occurrences_from_raster(rs, ts, seed)

  expect_equal(result1, result2)
})

test_that("sample_occurrences_from_raster raises an error for incorrect rs", {
  rs <- list() # Not a SpatRaster object
  ts <- 10

  expect_error(sample_occurrences_from_raster(rs, ts),
               "`rs` must be a SpatRaster object.")
})

test_that("sample_occurrences_from_raster raises an error for non-numeric ts", {
  rs <- create_sample_raster()
  ts <- "not_numeric"

  expect_error(sample_occurrences_from_raster(rs, ts),
               "`ts` must be a positive numeric vector.")
})

test_that("sample_occurrences_from_raster raises an error for negative ts", {
  rs <- create_sample_raster()
  ts <- -10

  expect_error(sample_occurrences_from_raster(rs, ts),
               "`ts` must be a positive numeric vector.")
})

test_that("sample_occurrences_from_raster raises an error for incorrect seed", {
  rs <- create_sample_raster()
  ts <- 10
  seed <- "not_numeric"

  expect_error(sample_occurrences_from_raster(rs, ts, seed),
               "`seed` must be a numeric vector of length 1 or NA.")
})

test_that("sample_occurrences_from_raster returns diff. results without seed", {
  rs <- create_sample_raster()
  ts <- 10

  result1 <- sample_occurrences_from_raster(rs, ts)
  result2 <- sample_occurrences_from_raster(rs, ts)

  expect_false(identical(result1, result2))
})

test_that("sample_occurrences_from_raster handles CRS correctly", {
  rs <- create_sample_raster()
  ts <- 10:20
  result <- sample_occurrences_from_raster(rs, ts)

  expect_equal(terra::crs(rs), sf::st_crs(result)$wkt)
})
