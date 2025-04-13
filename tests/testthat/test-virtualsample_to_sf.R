test_that("virtualsample_to_sf returns sf object with expected structure", {
  skip_if_not_installed("virtualspecies")
  skip_if_not_installed("terra")

  # Set up reproducible virtual species sample
  set.seed(1)
  bio <- terra::rast(
    system.file("external/bio.tif", package = "virtualspecies"))
  vs <- virtualspecies::generateRandomSp(bio)
  vs_limited <- virtualspecies::limitDistribution(
    vs, geographical.limit = "continent", area = "Europe")
  virtual_sample <- virtualspecies::sampleOccurrences(vs_limited, n = 10)

  # Add required original.distribution.raster
  env_layers <- vs$details$parameters$environmental.layers
  virtual_sample$original.distribution.raster <- env_layers

  # Run function
  sf_out <- virtualsample_to_sf(virtual_sample)

  # Check output class
  expect_s3_class(sf_out, "sf")

  # Check columns
  expect_true(all(c("id", "observed", "geometry") %in% names(sf_out)))
  expect_type(sf_out$observed, "logical")
  expect_equal(nrow(sf_out), nrow(virtual_sample$sample.points))
})

test_that("virtualsample_to_sf extracts raster values correctly", {
  skip_if_not_installed("virtualspecies")
  skip_if_not_installed("terra")

  set.seed(1)
  bio <- terra::rast(
    system.file("external/bio.tif", package = "virtualspecies"))
  vs <- virtualspecies::generateRandomSp(bio)
  vs_limited <- virtualspecies::limitDistribution(
    vs, geographical.limit = "continent", area = "Europe")
  virtual_sample <- virtualspecies::sampleOccurrences(vs_limited, n = 10)
  env_layers <- vs$details$parameters$environmental.layers
  virtual_sample$original.distribution.raster <- env_layers

  # Add a raster layer to extract (e.g. suitability)
  suitab <- vs$suitab.raster
  names(suitab) <- "suitability"

  sf_out <- virtualsample_to_sf(virtual_sample, raster_lyr = suitab)

  # Check if the suitability column is present
  expect_true("suitability" %in% names(sf_out))
})

test_that("virtualsample_to_sf handles missing raster_lyr argument", {
  skip_if_not_installed("virtualspecies")

  # Minimal virtual sample input
  pts <- data.frame(
    x = c(1, 2), y = c(3, 4),
    Observed = c(1, NA), Real = c(1, 0)
  )
  rast <- terra::rast(nrows = 10, ncols = 10,
                      xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  terra::values(rast) <- runif(100)

  virtual_sample <- list(
    sample.points = pts,
    original.distribution.raster = rast)

  out <- virtualsample_to_sf(virtual_sample)

  expect_s3_class(out, "sf")
  expect_equal(out$observed, c(TRUE, FALSE))
})

test_that("virtualsample_to_sf removes Real and Observed columns", {
  rast <- terra::rast(nrows = 10, ncols = 10,
                      xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  terra::values(rast) <- runif(100)

  sample_data <- data.frame(
    x = c(1, 2),
    y = c(3, 4),
    Observed = c(NA, 1),
    Real = c(1, 0)
  )

  virtual_sample <- list(
    sample.points = sample_data,
    original.distribution.raster = rast)

  out <- virtualsample_to_sf(virtual_sample)

  expect_false("Real" %in% names(out))
  expect_false("Observed" %in% names(out))
  expect_true("observed" %in% names(out))
})
