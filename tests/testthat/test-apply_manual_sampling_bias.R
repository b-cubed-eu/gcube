## Prepare example datasets
occurrences <- data.frame(
  lon = c(
    -67.7872072532773, -32.589017059654, -176.231839740649,
    -113.81417135708, 123.382554799318, -96.7817584611475,
    -93.9240159653127, -152.391180479899, -91.5394759085029,
    83.5686739906669
  ),
  lat = c(
    61.1581976618618, -33.7593303155154, 37.4922579992563,
    -42.2967949043959,
    16.9817749271169, -3.36783591192216, -42.2941083367914,
    11.6262782597914,
    74.37388014514, 72.3373901098967
  ),
  time_point = 1
)
points_sf1 <- sf::st_as_sf(occurrences, coords = c("lon", "lat"))

# Create raster grid
grid_sf_withoutweights <- sf::st_make_grid(points_sf1) %>% sf::st_sf()

# Bias weights between 0 and 1
bias_weights01_sf <- grid_sf_withoutweights %>%
  dplyr::mutate(
    bias_weight = runif(nrow(grid_sf_withoutweights), min = 0, max = 1)
  )

# bias_weights larger than 1
bias_weights_integers_sf <- grid_sf_withoutweights %>%
  mutate(bias_weight = rpois(nrow(grid_sf_withoutweights), 5))

# Bias weights without geometry
bias_weights_nogeom <- st_drop_geometry(bias_weights01_sf)


## Unit Tests
test_that("Function adds a bias_weight column with weights between 0 and 1", {
  result <- apply_manual_sampling_bias(points_sf1, bias_weights01_sf)
  expect_true("bias_weight" %in% colnames(result))
  expect_true(all(result$bias_weight >= 0 & result$bias_weight <= 1))

  # with integer weights
  result2 <- apply_manual_sampling_bias(points_sf1, bias_weights_integers_sf)
  expect_true("bias_weight" %in% colnames(result2))
  expect_true(all(result2$bias_weight >= 0 & result2$bias_weight <= 1))
})

test_that("Function throws an error if occurrences_sf is not an sf object", {
  expect_error(apply_manual_sampling_bias(occurrences, bias_weights01_sf),
               "`occurrences_sf` must be an sf object.")
})

test_that("Function throws an error if bias_weights is not an sf object", {
  expect_error(apply_manual_sampling_bias(points_sf1, bias_weights_nogeom),
               "`bias_weights` must be an sf object.")
})

test_that("Function throws an error if occurrences_sf does not have POINT", {
  line_sf <- st_as_sf(data.frame(
    geometry = st_sfc(st_linestring(matrix(c(0, 0, 1, 1), ncol = 2)))),
    crs = 4326)
  expect_error(apply_manual_sampling_bias(line_sf, bias_weights01_sf),
               "`occurrences_sf` must be an sf object.")
})

test_that("Function throws an error if bias_weights does not have POLYGON ", {
  point_sf <- st_as_sf(data.frame(
    geometry = st_sfc(st_point(c(0, 0)))),
    crs = 4326)
  expect_error(apply_manual_sampling_bias(points_sf1, point_sf),
               "`bias_weights` must be an sf object.")
})

test_that("Function throws error if bias_weights does not have bias_weight", {
  grid_no_bias <- bias_weights01_sf %>%
    select(-bias_weight)
  expect_error(apply_manual_sampling_bias(points_sf1, grid_no_bias),
               "`bias_weights` must have a column named `bias_weight`.")
})

test_that("Function throws an error if bias_weight values are not correct", {
  grid_invalid_bias <- bias_weights01_sf %>%
    mutate(bias_weight = rep(c("a", "b", "c", "d"), 25))
  expect_error(
    apply_manual_sampling_bias(points_sf1, grid_invalid_bias),
    paste("The column `bias_weight` must consist of numeric values between 0",
          "and 1, or positive integers.")
  )

  grid_negative_bias <- bias_weights01_sf %>%
    mutate(bias_weight = runif(nrow(bias_weights01_sf), min = -1, max = 0))
  expect_error(
    apply_manual_sampling_bias(points_sf1, grid_negative_bias),
    paste("The column `bias_weight` must consist of numeric values between 0",
          "and 1, or positive integers.")
  )
})

test_that("Function throws an error if the CRS of inputs not match", {
  grid_different_crs <- st_set_crs(bias_weights01_sf, 3857)
  expect_error(apply_manual_sampling_bias(points_sf1, grid_different_crs),
               "`bias_weights` must have the same CRS as `occurrences_sf`.")
})

test_that("Function throws an error if not all occurrences are within grid", {
  grid_small <- bias_weights01_sf[1,]
  expect_error(
    apply_manual_sampling_bias(points_sf1, grid_small),
    "`bias_weights` must be a grid that encompasses all occurrences.")
})

test_that("CRS of output matches CRS of input", {
  result <- apply_manual_sampling_bias(
    st_set_crs(points_sf1, 3857),
    st_set_crs(bias_weights01_sf, 3857))
  expect_equal(st_crs(result), st_crs(st_set_crs(points_sf1, 3857)))
})
