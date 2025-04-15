## Prepare example datasets
# Create four random points
n_points <- 4
xlim <- c(3841000, 3842000)
ylim <- c(3110000, 3112000)

occurrences_sf <- data.frame(
  lat = c(3110575, 3111577, 3110818, 3111766),
  long = c(3841940, 3841046, 3841528, 3841892)
) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 3035)

# Create bias_area polygon overlapping two of the points
selected_occurrences_sf <- sf::st_union(occurrences_sf[2:3, ])
bias_area <- sf::st_convex_hull(selected_occurrences_sf) %>%
  sf::st_buffer(dist = 100) %>%
  sf::st_as_sf()

# Set bias_strength
bias_strength <- 2


## Unit tests

test_that("arguments are of the right class", {
  # occurrences_sf is sf dataframe
  expect_error(
    apply_polygon_sampling_bias(
      data.frame(x = 1, y = 1),
      bias_area,
      bias_strength
    ),
    regexp = "`occurrences_sf` must be an sf object.",
    fixed = TRUE
  )
  expect_error(
    apply_polygon_sampling_bias(
      occurrences_sf = 1,
      bias_area,
      bias_strength
    ),
    regexp = "`occurrences_sf` must be an sf object.",
    fixed = TRUE
  )
  expect_error(
    apply_polygon_sampling_bias(
      occurrences_sf = "string",
      bias_area,
      bias_strength
    ),
    regexp = "`occurrences_sf` must be an sf object.",
    fixed = TRUE
  )

  # bias_area is sf dataframe
  expect_error(
    apply_polygon_sampling_bias(
      occurrences_sf,
      data.frame(x = 1, y = 1),
      bias_strength = 1
    ),
    regexp = "`bias_area` must be an sf object.",
    fixed = TRUE
  )
  expect_error(
    apply_polygon_sampling_bias(
      occurrences_sf,
      bias_area = 1,
      bias_strength = 1
    ),
    regexp = "`bias_area` must be an sf object.",
    fixed = TRUE
  )
  expect_error(
    apply_polygon_sampling_bias(
      occurrences_sf,
      bias_area = "string",
      bias_strength = 1
    ),
    regexp = "`bias_area` must be an sf object.",
    fixed = TRUE
  )
  #bias_area is an sf dataframe containing only polygons
  expect_error(apply_polygon_sampling_bias(occurrences_sf,
                                           bias_area = occurrences_sf,
                                           bias_strength = 1),
               regexp = "`bias_area` must be an sf object.",
               fixed = TRUE)

  # bias_strength is numeric
  expect_error(
    apply_polygon_sampling_bias(
      occurrences_sf,
      bias_area = bias_area,
      bias_strength = data.frame(x = 1, y = 1)
    ),
    regexp = "`bias_strength` must be a single positive number.",
    fixed = TRUE
  )
  expect_error(
    apply_polygon_sampling_bias(
      occurrences_sf,
      bias_area = bias_area,
      bias_strength = "string"
    ),
    regexp = "`bias_strength` must be a single positive number.",
    fixed = TRUE
  )
})

test_that("arguments are of the right length", {
  # bias_strength has length 1
  expect_error(
    apply_polygon_sampling_bias(
      occurrences_sf,
      bias_area,
      bias_strength = rep(3, 3)
    ),
    regexp = "`bias_strength` must be a single positive number.",
    fixed = TRUE
  )
})

test_that("bias_weight column contains values between 0 and 1", {
  # Test whether bias_weight column contains correct values
  result <- apply_polygon_sampling_bias(
    occurrences_sf,
    bias_area,
    bias_strength = bias_strength
  )
  expect_true(all(result$bias_weight > 0 & result$bias_weight < 1))
})

test_that("only one column (bias_weight) is added to occurrences_sf", {
  # Test whether only one column (bias_weight) is added
  result <- apply_polygon_sampling_bias(
    occurrences_sf,
    bias_area,
    bias_strength = bias_strength
  )
  # Checking if only one column is added
  expect_equal(ncol(result), ncol(occurrences_sf) + 1)
  # Checking if the added column is bias_weight
  expect_true("bias_weight" %in% colnames(result))
})

test_that("CRS of output matches CRS of input", {
  result <- apply_polygon_sampling_bias(occurrences_sf,
                                        bias_area,
                                        bias_strength = bias_strength)
  expect_equal(st_crs(result), st_crs(occurrences_sf))
})

test_that("Function correctly handles bias_strength of 1", {
  result <- apply_polygon_sampling_bias(occurrences_sf,
                                        bias_area,
                                        bias_strength = 1)
  expect_true(all(result$bias_weight == 0.5))
})

test_that("Function adds a column correctly for positive bias_strength", {
  result <- apply_polygon_sampling_bias(occurrences_sf,
                                        bias_area,
                                        bias_strength)
  expect_true("bias_weight" %in% colnames(result))
  expect_true(all(result$bias_weight >= 0))
  expect_equal(sum(result$bias_weight), n_points / 2)
})

test_that("Function adds a column correctly for bias_strength less than 1", {
  result <- apply_polygon_sampling_bias(occurrences_sf,
                                        bias_area,
                                        bias_strength = 0.5)
  expect_true("bias_weight" %in% colnames(result))
  expect_true(all(result$bias_weight >= 0))
  expect_equal(sum(result$bias_weight), n_points / 2)
})
