## Prepare example datasets
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

# Convert the polygon to an sf object
plgn_sf <- st_sfc(plgn)

# Assign a CRS to the sf object
example_crs <- 4326
plgn_sf <- st_sf(geometry = plgn_sf, crs = example_crs)

## Unit Tests
test_that("create_spatial_pattern works with default parameters", {
  result <- create_spatial_pattern(polygon = plgn, resolution = 0.1)
  expect_s4_class(result, "SpatRaster")
  expect_equal(
    as.vector(terra::ext(result))[order(names(as.vector(terra::ext(result))))],
    sf::st_bbox(plgn)[order(names(sf::st_bbox(plgn)))]
  )
  expect_equal(unique(terra::res(result)), 0.1)
})

test_that("create_spatial_pattern works with random pattern", {
  result <- create_spatial_pattern(
    polygon = plgn, resolution = 0.1, spatial_pattern = "random"
  )
  expect_s4_class(result, "SpatRaster")
  expect_equal(
    as.vector(terra::ext(result))[order(names(as.vector(terra::ext(result))))],
    sf::st_bbox(plgn)[order(names(sf::st_bbox(plgn)))]
  )
  expect_equal(unique(terra::res(result)), 0.1)
})

test_that("create_spatial_pattern works with clustered pattern", {
  result <- create_spatial_pattern(
    polygon = plgn, resolution = 0.1, spatial_pattern = "clustered"
  )
  expect_s4_class(result, "SpatRaster")
  expect_equal(
    as.vector(terra::ext(result))[order(names(as.vector(terra::ext(result))))],
    sf::st_bbox(plgn)[order(names(sf::st_bbox(plgn)))]
  )
  expect_equal(unique(terra::res(result)), 0.1)
})

test_that("create_spatial_pattern works with user-defined clustering", {
  result <- create_spatial_pattern(
    polygon = plgn, resolution = 0.1, spatial_pattern = 5
  )
  expect_s4_class(result, "SpatRaster")
  expect_equal(
    as.vector(terra::ext(result))[order(names(as.vector(terra::ext(result))))],
    sf::st_bbox(plgn)[order(names(sf::st_bbox(plgn)))]
  )
  expect_equal(unique(terra::res(result)), 0.1)
})

test_that("create_spatial_pattern respects seed for reproducibility", {
  result1 <- create_spatial_pattern(
    polygon = plgn, resolution = 0.1, spatial_pattern = "random", seed = 123
  )
  result2 <- create_spatial_pattern(
    polygon = plgn, resolution = 0.1, spatial_pattern = "random", seed = 123
  )
  expect_true(terra::identical(result1, result2))
})

test_that("create_spatial_pattern handles different resolutions", {
  result <- create_spatial_pattern(
    polygon = plgn, resolution = 0.5, spatial_pattern = "random"
  )
  expect_s4_class(result, "SpatRaster")
  expect_equal(
    as.vector(terra::ext(result))[order(names(as.vector(terra::ext(result))))],
    sf::st_bbox(plgn)[order(names(sf::st_bbox(plgn)))]
  )
  expect_equal(unique(terra::res(result)), 0.5)
})

test_that("create_spatial_pattern handles multiple simulations", {
  result <- create_spatial_pattern(
    polygon = plgn, resolution = 0.1, spatial_pattern = "random", n_sim = 3
  )
  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 3)
  expect_equal(unique(terra::res(result)), 0.1)
  expect_equal(
    as.vector(terra::ext(result))[order(names(as.vector(terra::ext(result))))],
    sf::st_bbox(plgn)[order(names(sf::st_bbox(plgn)))]
  )
  expect_equal(unique(terra::res(result)), 0.1)
})

test_that("create_spatial_pattern handles CRS correctly", {
  result <- create_spatial_pattern(polygon = plgn_sf, resolution = 0.1)
  expect_s4_class(result, "SpatRaster")
  expect_equal(
    as.vector(terra::ext(result))[order(names(as.vector(terra::ext(result))))],
    sf::st_bbox(plgn)[order(names(sf::st_bbox(plgn_sf)))]
  )
  expect_equal(unique(terra::res(result)), 0.1)
  expect_equal(terra::crs(result), sf::st_crs(plgn_sf)$wkt)
})

test_that("create_spatial_pattern throws errors for invalid inputs", {
  expect_error(
    create_spatial_pattern(polygon = "not_a_polygon", resolution = 0.1),
    "`polygon` must be an sf object with POLYGON geometry.")
  expect_error(
    create_spatial_pattern(polygon = plgn, resolution = -1),
    "`resolution` must be a single positive number.")
  expect_error(
    create_spatial_pattern(polygon = plgn,
                           resolution = 0.1,
                           spatial_pattern = "invalid_pattern"),
    paste("`spatial_pattern` must be one of 'random', 'clustered', or a single",
          "number larger or equal to 1.")
  )
  expect_error(
    create_spatial_pattern(polygon = plgn, resolution = 0.1, seed = "string"),
    "`seed` must be a numeric vector of length 1 or NA.")
  expect_error(
    create_spatial_pattern(polygon = plgn, resolution = 0.1, n_sim = 0),
    "`n_sim` must be a single positive integer.")
})
