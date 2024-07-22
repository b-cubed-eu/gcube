test_that("sample_occurrences_from_raster() returns valid sf POINT geometry", {
  # build timeseries and raster to test on
  test_raster <-
    terra::rast(
      ncol = 50,
      nrow = 50,
      xmin = 0,
      xmax = 50,
      ymin = 0,
      ymax = 50
    )
  terra::values(test_raster) <- 1:terra::ncell(test_raster)
  timeseries <- c(20, 40, 60)

  expect_s3_class(
    sample_occurrences_from_raster(rs = test_raster, ts = timeseries),
    "data.frame"
  )

  expect_s3_class(
    sample_occurrences_from_raster(rs = test_raster, ts = timeseries),
    "sf"
  )
})

test_that("sample_occurrences_from_raster() fails on invalid input argument", {
  # build raster to test on
  test_raster <-
    terra::rast(
      ncol = 50,
      nrow = 50,
      xmin = 0,
      xmax = 50,
      ymin = 0,
      ymax = 50
    )
  terra::values(test_raster) <- 1:terra::ncell(test_raster)

  expect_error(
    sample_occurrences_from_raster(data.frame(5:9)),
    regexp = "`rs` must be a SpatRaster object.",
    fixed = TRUE
  )

  expect_error(
    sample_occurrences_from_raster(test_raster, "not a numeric"),
    regexp = "`ts` must be a positive numeric vector.",
    fixed = TRUE
  )
})
