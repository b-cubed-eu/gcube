# Simulate some occurrence data with coordinates and time points
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
  time_point = 0
)
points_sf1 <- sf::st_as_sf(occurrences, coords = c("lon", "lat"))

## dataset without geometry
points_sf2 <- points_sf1 %>%
  sf::st_drop_geometry()

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

# bias_weights as a data fram
bias_weights_df <- as.data.frame(bias_weights01_sf)

# bias_weights without geometry
bias_weights_nogeom <- bias_weights01_sf %>%
  sf::st_drop_geometry()

# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # occurrences_sf is an sf object
  expect_error(
    apply_manual_sampling_bias(
      occurrences_sf = occurrences,
      bias_weights = bias_weights01_sf
    ),
    "`occurrences_sf` must be an sf object."
  )
  expect_error(
    apply_manual_sampling_bias(
      occurrences_sf = points_sf2,
      bias_weights = bias_weights01_sf
    ),
    "`occurrences_sf` must be an sf object."
  )
  expect_error(
    apply_manual_sampling_bias(
      occurrences_sf = "string",
      bias_weights = bias_weights01_sf
    ),
    "`occurrences_sf` must be an sf object."
  )

  # bias_weights is an sf object
  expect_error(
    apply_manual_sampling_bias(
      occurrences_sf = points_sf1,
      bias_weights = bias_weights_df
    ),
    "`bias_weights` must be an sf object."
  )
  expect_error(
    apply_manual_sampling_bias(
      occurrences_sf = points_sf1,
      bias_weights = bias_weights_nogeom
    ),
    "`bias_weights` must be an sf object."
  )

  # bias_weights has a column named bias_weight
  expect_error(
    apply_manual_sampling_bias(
      occurrences_sf = points_sf1,
      bias_weights = grid_sf_withoutweights
    ),
    "`bias_weights` must have a column named `bias_weight`."
  )
  }
)

## expect right output format
test_that("apply_manual_sampling_bias() returns sf object", {
  expect_s3_class(
    apply_manual_sampling_bias(points_sf1, bias_weights01_sf),
    "data.frame"
  )
  expect_s3_class(
    apply_manual_sampling_bias(points_sf1, bias_weights01_sf),
    "sf"
  )
})
test_that("apply_manual_sampling_bias() returns bias_weight column", {
  expect_named(
    apply_manual_sampling_bias(points_sf1, bias_weights01_sf),
    c("time_point", "bias_weight", "geometry")
  )
  expect_type(
    apply_manual_sampling_bias(points_sf1, bias_weights_integers_sf) %>%
      dplyr::pull("bias_weight"),
    "double"
  )
  expect_true(is.numeric(x = dplyr::pull(
    apply_manual_sampling_bias(points_sf1, bias_weights01_sf),
    "bias_weight"
  )))
})
