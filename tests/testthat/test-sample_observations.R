# Simulate some occurrence data with coordinates and time points
occurrences <- data.frame(
  lon = c(
    -67.7872072532773, -32.589017059654, -176.231839740649, -113.81417135708,
    123.382554799318, -96.7817584611475, -93.9240159653127, -152.391180479899,
    -91.5394759085029, 83.5686739906669),
  lat = c(
    61.1581976618618, -33.7593303155154, 37.4922579992563, -42.2967949043959,
    16.9817749271169, -3.36783591192216, -42.2941083367914, 11.6262782597914,
    74.37388014514, 72.3373901098967),
  time_point = 0
)
points_sf1 <- sf::st_as_sf(occurrences, coords = c("lon", "lat"))

## dataset without geometry
points_sf2 <- points_sf1 %>%
  sf::st_drop_geometry()


# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # occurrences is an sf object
  expect_error(sample_observations(occurrences = occurrences),
               regexp = "`occurrences` must be an sf object.",
               fixed = TRUE)
  expect_error(sample_observations(occurrences = points_sf2),
               regexp = "`occurrences` must be an sf object.",
               fixed = TRUE)
  expect_error(sample_observations(occurrences = "string"),
               regexp = "`occurrences` must be an sf object.",
               fixed = TRUE)

  # detection_probability is a numeric value
  expect_error(
    sample_observations(points_sf1, "1"),
    regexp = "`detection_probability` must be a numeric value between 0 and 1.",
    fixed = TRUE)
  expect_error(
    sample_observations(points_sf1,
                        detection_probability = TRUE),
    regexp = "`detection_probability` must be a numeric value between 0 and 1.",
    fixed = TRUE)

  # sampling_bias is a character vector
  expect_error(
    sample_observations(points_sf1, 0.5, TRUE),
    regexp = "`sampling_bias` must be one of 'no_bias', 'polygon', 'manual'.",
    fixed = TRUE)

  # seed is a numeric value
  expect_error(sample_observations(points_sf1, seed = TRUE),
               regexp = "`seed` must be a numeric vector of length 1",
               fixed = TRUE)
  expect_error(sample_observations(points_sf1, seed = "123"),
             regexp = "`seed` must be a numeric vector of length 1",
             fixed = TRUE)
})
