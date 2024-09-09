# Prepare example datasets
## number of points and extend
n_points <- 4
xlim <- c(3841000, 3842000)
ylim <- c(3110000, 3112000)

## dataset without coordinateUncertaintyInMeters
observations_sf1 <- data.frame(
  lat = runif(n_points, ylim[1], ylim[2]),
  long = runif(n_points, xlim[1], xlim[2]),
  time_point = 1
  ) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 3035)

## dataset with coordinateUncertaintyInMeters
coordinate_uncertainty <- c(24.32870, 53.96961, 28.16026, 43.24885)
observations_sf2 <- observations_sf1 %>%
  dplyr::mutate(coordinateUncertaintyInMeters = coordinate_uncertainty)

## dataset without geometry
observations_sf3 <- observations_sf2 %>%
  sf::st_drop_geometry()

## dataset without time points
observations_sf4 <- observations_sf2[-1]

# Unit tests
## expect errors
test_that("arguments are of the right class", {
  # observations are sf dataframe
  expect_error(sample_from_binormal_circle(observations_sf3),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)
  expect_error(sample_from_binormal_circle(observations = 2),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)
  expect_error(sample_from_binormal_circle(observations = "string"),
               regexp = "`observations` must be an sf object",
               fixed = TRUE)

  # pnorm is numeric between 0 and 1
  expect_error(sample_from_binormal_circle(observations_sf1, p_norm = "0.95"),
               regexp = "`pnorm` must be a numeric value between 0 and 1.",
               fixed = TRUE)
  expect_error(sample_from_binormal_circle(observations_sf2, p_norm = "0.95"),
               regexp = "`pnorm` must be a numeric value between 0 and 1.",
               fixed = TRUE)
  expect_error(sample_from_binormal_circle(observations_sf1, p_norm = -0.5),
               regexp = "`pnorm` must be a numeric value between 0 and 1.",
               fixed = TRUE)
  expect_error(sample_from_binormal_circle(observations_sf2, p_norm = -0.5),
               regexp = "`pnorm` must be a numeric value between 0 and 1.",
               fixed = TRUE)

  # seed is numeric
  expect_error(sample_from_binormal_circle(observations_sf1, seed = "123"),
               regexp = "`seed` must be a numeric vector of length 1 or NA.",
               fixed = TRUE)
  expect_error(sample_from_binormal_circle(observations_sf2, seed = "123"),
               regexp = "`seed` must be a numeric vector of length 1 or NA.",
               fixed = TRUE)
})

test_that("arguments are of the right length", {
  # pnorm is length 1
  expect_error(sample_from_binormal_circle(observations_sf1,
                                           p_norm = seq(0.1, 0.3, 0.1)),
               regexp = "`pnorm` must be a numeric value between 0 and 1.",
               fixed = TRUE)
  expect_error(sample_from_binormal_circle(observations_sf2,
                                           p_norm = seq(0.1, 0.3, 0.1)),
               regexp = "`pnorm` must be a numeric value between 0 and 1.",
               fixed = TRUE)

  # seed has length 1
  expect_error(sample_from_binormal_circle(observations_sf1, seed = 1:3),
               regexp = "`seed` must be a numeric vector of length 1 or NA.",
               fixed = TRUE)
  expect_error(sample_from_binormal_circle(observations_sf2, seed = 1:3),
               regexp = "`seed` must be a numeric vector of length 1 or NA.",
               fixed = TRUE)
})

## expect warnings
test_that("warning if coordinateUncertaintyInMeters column is not present", {
  expect_warning(
    sample_from_binormal_circle(observations_sf1),
    regexp = paste(
      "No column `coordinateUncertaintyInMeters` present!",
      "Assuming no uncertainty around observations.",
      sep = "\n"
    ),
    fixed = TRUE)
})

test_that("warning if time_point column is not present", {
  expect_warning(
    sample_from_uniform_circle(observations_sf4),
    regexp = paste(
      "No column `time_point` present!",
      "Assuming only a single time point.",
      sep = "\n"
    ),
    fixed = TRUE)
})

## expected outputs
test_that("output class is correct", {
  suppressWarnings({
    expect_s3_class(sample_from_binormal_circle(observations_sf1),
                    class = "sf")
    expect_s3_class(sample_from_binormal_circle(observations_sf1),
                    class = "data.frame")
  })
  expect_s3_class(sample_from_binormal_circle(observations_sf2),
                  class = "sf")
  expect_s3_class(sample_from_binormal_circle(observations_sf2),
                  class = "data.frame")
})

test_that("correct column names present", {
  suppressWarnings({
    expect_contains(names(sample_from_binormal_circle(observations_sf1)),
                    c("coordinateUncertaintyInMeters", "geometry"))
  })
  expect_contains(names(sample_from_binormal_circle(observations_sf2)),
                  c("coordinateUncertaintyInMeters", "geometry"))
})

test_that("coordinateUncertaintyInMeters column is handled correctly", {
  # in case of missing initial coordinateUncertaintyInMeters column
  ## no seed
  suppressWarnings({
    expect_equal(
      sample_from_binormal_circle(observations_sf1) %>%
        dplyr::pull(coordinateUncertaintyInMeters),
      rep(0, nrow(observations_sf1)))
    ## different seeds
    expect_equal(
      sample_from_binormal_circle(observations_sf1, seed = 123) %>%
        dplyr::pull(coordinateUncertaintyInMeters),
      rep(0, nrow(observations_sf1)))
    expect_equal(
      sample_from_binormal_circle(observations_sf1, seed = 456) %>%
        dplyr::pull(coordinateUncertaintyInMeters),
      rep(0, nrow(observations_sf1)))
  })

  # in case of provided initial coordinateUncertaintyInMeters column
  ## no seed
  expect_equal(
    sample_from_binormal_circle(observations_sf2) %>%
      dplyr::pull(coordinateUncertaintyInMeters),
    observations_sf2 %>%
      dplyr::pull(coordinateUncertaintyInMeters))
  ## different seeds
  expect_equal(
    sample_from_binormal_circle(observations_sf2, seed = 123) %>%
      dplyr::pull(coordinateUncertaintyInMeters),
    observations_sf2 %>%
      dplyr::pull(coordinateUncertaintyInMeters))
  expect_equal(
    sample_from_binormal_circle(observations_sf2, seed = 456) %>%
      dplyr::pull(coordinateUncertaintyInMeters),
    observations_sf2 %>%
      dplyr::pull(coordinateUncertaintyInMeters))
})

# This function calculates if the distances between the sampled points and the
# original point are not larger than their coordinate uncertainty
test_smaller_distances <- function(observations, seed = NA) {
  sample_dists <- sample_from_binormal_circle(observations, seed = seed) %>%
    dplyr::mutate(dist = sf::st_distance(.data$geometry, observations,
                              by_element = TRUE),
           dist = as.numeric(dist)) %>%
    dplyr::pull(dist)
  test_dists_df <- observations %>%
    sf::st_drop_geometry() %>%
    mutate(dist = sample_dists,
           test = dist < .data$coordinateUncertaintyInMeters)

  return(all(test_dists_df$test))
}

test_that("distance to new point falls within coordinate uncertainty", {
  # in case of missing initial coordinateUncertaintyInMeters column
  suppressWarnings({
    ## no seed
    expect_equal(
      sample_from_binormal_circle(observations_sf1) %>%
        dplyr::mutate(dist = sf::st_distance(geometry, observations_sf1,
                                  by_element = TRUE),
               dist = as.numeric(dist)) %>%
        dplyr::pull(dist),
      rep(0, nrow(observations_sf1)))
    ## different seeds
    expect_equal(
      sample_from_binormal_circle(observations_sf1, seed = 123) %>%
        dplyr::mutate(dist = sf::st_distance(geometry, observations_sf1,
                                  by_element = TRUE),
               dist = as.numeric(dist)) %>%
        dplyr::pull(dist),
      rep(0, nrow(observations_sf1)))
    expect_equal(
      sample_from_binormal_circle(observations_sf1, seed = 456) %>%
        dplyr::mutate(dist = sf::st_distance(geometry, observations_sf1,
                                  by_element = TRUE),
               dist = as.numeric(dist)) %>%
        dplyr::pull(dist),
      rep(0, nrow(observations_sf1)))
  })

  # in case of provided initial coordinateUncertaintyInMeters column
  ## no seed
  expect_true(test_smaller_distances(observations_sf2))
  ## different seeds
  expect_true(test_smaller_distances(observations_sf2, seed = 123))
  expect_true(test_smaller_distances(observations_sf2, seed = 456))
})
