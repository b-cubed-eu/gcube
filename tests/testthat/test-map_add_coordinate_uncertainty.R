## Prepare example datasets
# Create a simple polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

# Specify dataframe for 3 species with custom function arguments
# Dataframe with column names equal to arguments for simple polygon
species_dataset_df1 <- tibble(
  taxonID = c("species1", "species2", "species3"),
  species_range = rep(list(plgn), 3),
  initial_average_occurrences = c(50, 100, 500),
  n_time_points = rep(6, 3),
  temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
  sd_step = c(1, 1, NA),
  spatial_pattern = "random",
  detection_probability = c(0.8, 0.9, 1),
  invert = FALSE,
  coords_uncertainty_meters = c(25, 30, 50),
  seed = 123)

# Dataframe with custom column names and named list for argument conversion for
# simple polygon. Create named list for argument conversion.
species_dataset_df2 <- species_dataset_df1 %>%
  rename(polygon = species_range,
         sd = sd_step,
         det_prob = detection_probability,
         inv = invert,
         coord_uncertainty = coords_uncertainty_meters)

arg_conv_list <- list(
    species_range = "polygon",
    sd_step = "sd",
    detection_probability = "det_prob",
    invert = "inv",
    coords_uncertainty_meters = "coord_uncertainty"
  )

# Map simulate occurrences
sim_occ1 <- map_simulate_occurrences(
  df = species_dataset_df1)
sim_occ2 <- map_simulate_occurrences(
  df = species_dataset_df2,
  arg_list = arg_conv_list)

# Map sample observations
samp_obs1 <- map_sample_observations(
  df = sim_occ1)
samp_obs2 <- map_sample_observations(
  df = sim_occ2,
  arg_list = arg_conv_list)

# Map sample observations
filter_obs1 <- map_filter_observations(
  df = samp_obs1)
filter_obs2 <- map_filter_observations(
  df = samp_obs2,
  arg_list = arg_conv_list)


## Unit tests

test_that("map_add_coordinate_uncertainty works with simple column names", {
  # Test with nested is TRUE
  obs_uncertainty_nested <- map_add_coordinate_uncertainty(df = filter_obs1)

  # Are previous column names the same?
  expect_true("observations" %in% colnames(obs_uncertainty_nested))
  expect_equal(sort(colnames(filter_obs1)),
               sort(colnames(obs_uncertainty_nested)))
  # Is the column a list-column?
  expect_true(inherits(obs_uncertainty_nested$observations, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(obs_uncertainty_nested$observations, inherits, "sf")))
  # Does each sf object contain an extra column coordinateUncertaintyInMeters
  expect_true(
    all(
      sapply(obs_uncertainty_nested$observations, function(sf) {
          "coordinateUncertaintyInMeters" %in% colnames(sf)
        })
    )
  )

  # Test with nested is FALSE
  obs_uncertainty_unnested <- map_add_coordinate_uncertainty(df = filter_obs1,
                                                             nested = FALSE)

  # Do we have unnested successfully?
  expect_true(nrow(obs_uncertainty_unnested) > nrow(obs_uncertainty_nested))
  obs_uncertainty_unnested_test <- tidyr::unnest(
    obs_uncertainty_nested,
    cols = "observations",
    names_repair = "minimal")
  obs_uncertainty_unnested_test <- obs_uncertainty_unnested_test[
    , !duplicated(t(obs_uncertainty_unnested_test))
  ]
  expect_equal(obs_uncertainty_unnested_test,
               subset(obs_uncertainty_unnested, select = -observations))
})

test_that("map_add_coordinate_uncertainty works with arg_list", {
  # Test with arg_list
  obs_uncertainty_nested <- map_add_coordinate_uncertainty(
    df = filter_obs2,
    arg_list = arg_conv_list)

  # Are previous column names the same?
  expect_true("observations" %in% colnames(obs_uncertainty_nested))
  expect_equal(sort(colnames(filter_obs2)),
               sort(colnames(obs_uncertainty_nested)))
  # Is the column a list-column?
  expect_true(inherits(obs_uncertainty_nested$observations, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(obs_uncertainty_nested$observations, inherits, "sf")))
  # Does each sf object contain an extra column coordinateUncertaintyInMeters
  expect_true(
    all(
      sapply(obs_uncertainty_nested$observations, function(sf) {
        "coordinateUncertaintyInMeters" %in% colnames(sf)
      })
    )
  )
})

test_that("map_add_coordinate_uncertainty handles invalid inputs", {
  # Invalid dataframe input
  expect_error(map_add_coordinate_uncertainty(df = list(), nested = TRUE),
               "`df` must be a dataframe.")

  # Invalid nested argument
  expect_error(map_add_coordinate_uncertainty(df = sim_occ1,
                                              nested = "TRUE"),
               "`nested` must be a logical vector of length 1.")

  # Invalid arg_list
  invalid_arg_list <- list(
      species_range = "polygon",
      sd_step = "sd",
      detection_probability = "det_prob",
      invert = "inv",
      coords_uncertainty_meters = 123
    )
  expect_error(
    map_add_coordinate_uncertainty(df = sim_occ2,
                                   arg_list = invalid_arg_list),
    "`arg_list` must be named list containing one string for each value."
  )

  invalid_arg_list2 <- list(
      species_range = "polygon",
      sd_step = "sd",
      detection_probability = "det_prob",
      invert = "inv",
      coords_uncertainty_meters = "coord_uncert"
    )
  expect_error(
    map_add_coordinate_uncertainty(df = sim_occ2,
                                   arg_list = invalid_arg_list2),
    "You have provided column names in `arg_list` that are not present in `df`."
  )
})
