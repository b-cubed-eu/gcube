## Prepare example datasets
# Create a simple polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

# Create a polygon with a road
polygon <- st_polygon(list(cbind(c(500, 1000, 1000, 600, 200, 100, 500),
                                 c(200, 100, 700, 1000, 900, 500, 200))))
road_width <- 50
road_points <- rbind(c(100, 500), c(1000, 500))
road_polygon <- st_linestring(road_points) %>%
  st_buffer(road_width) %>%
  st_intersection(polygon) %>%
  st_polygon() %>%
  st_sfc() %>%
  st_as_sf() %>%
  rename(geometry = x)


# Specify dataframe for 3 species with custom function arguments
# Dataframe with column names equal to arguments for simple polygon
species_dataset_df1 <- tibble(
  taxonID = c("species1", "species2", "species3"),
  plgn = rep(list(plgn), 3),
  initial_average_abundance = c(50, 100, 500),
  n_time_points = rep(6, 3),
  temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
  sd_step = c(1, 1, NA),
  spatial_autocorr = "random",
  detection_probability = c(0.8, 0.9, 1),
  seed = 123)

# Dataframe with custom column names and named list for argument conversion for
# simple polygon. Create named list for argument conversion.
species_dataset_df2 <- species_dataset_df1 %>%
  rename(polygon = plgn,
         sd = sd_step,
         det_prob = detection_probability)

arg_conv_list <- list(
    plgn = "polygon",
    sd_step = "sd",
    detection_probability = "det_prob"
  )

# Dataframe with column names equal to arguments for road polygon
species_dataset_df3 <- tibble(
  taxonID = c("species1", "species2", "species3"),
  plgn = rep(list(plgn), 3),
  initial_average_abundance = c(50, 100, 500),
  n_time_points = rep(6, 3),
  temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
  sd_step = c(1, 1, NA),
  spatial_autocorr = "random",
  detection_probability = c(0.8, 0.9, 1),
  sampling_bias = "polygon",
  bias_area = rep(list(road_polygon), 3),
  bias_strength = c(0.1, 0.2, 0.3),
  seed = 123)


# Map simulate occurrences
sim_occ1 <- map_simulate_occurrences(
  df = species_dataset_df1)
sim_occ2 <- map_simulate_occurrences(
  df = species_dataset_df2,
  arg_list = arg_conv_list)
sim_occ3 <- map_simulate_occurrences(
  df = species_dataset_df3)

## Unit tests

test_that("map_sample_observations works with simple column names and plgn", {
  # Test with nested is TRUE
  sample_obs_nested <- map_sample_observations(df = sim_occ1)

  # Are previous column names retained and one extra column name created?
  expect_true("observations_total" %in% colnames(sample_obs_nested))
  expect_equal(sort(c(colnames(sim_occ1), "observations_total")),
               sort(colnames(sample_obs_nested)))
  # Is the new column a list-column?
  expect_true(inherits(sample_obs_nested$observations_total, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(sample_obs_nested$observations_total, inherits, "sf")))

  # Test with nested is FALSE
  sample_obs_unnested <- map_sample_observations(df = sim_occ1, nested = FALSE)

  # Is the occurrence column created?
  expect_false("observations_total" %in% colnames(sample_obs_unnested))
  # Do we have unnested successfully?
  expect_true(nrow(sample_obs_unnested) > nrow(sample_obs_nested))
  sample_obs_unnested_test <- tidyr::unnest(
    sample_obs_nested,
    cols = "observations_total",
    names_repair = "minimal")
  sample_obs_unnested_test <- sample_obs_unnested_test[
      , !duplicated(t(sample_obs_unnested_test))
    ]
  expect_equal(sample_obs_unnested_test, sample_obs_unnested)
})

test_that("map_sample_observations works with arg_list for renaming columns", {
  # Test with arg_list
  sample_obs_nested <- map_sample_observations(df = sim_occ2,
                                               arg_list = arg_conv_list)

  # Are previous column names retained and one extra column name created?
  expect_true("observations_total" %in% colnames(sample_obs_nested))
  expect_equal(sort(c(colnames(sim_occ2), "observations_total")),
               sort(colnames(sample_obs_nested)))
  # Is the new column a list-column?
  expect_true(inherits(sample_obs_nested$observations_total, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(sample_obs_nested$observations_total, inherits, "sf")))
})

test_that("map_sample_observations works with complex arguments", {
  # Test with nested is TRUE
  sample_obs_nested <- map_sample_observations(df = sim_occ3)

  # Are previous column names retained and one extra column name created?
  expect_true("observations_total" %in% colnames(sample_obs_nested))
  expect_equal(sort(c(colnames(sim_occ3), "observations_total")),
               sort(colnames(sample_obs_nested)))
  # Is the new column a list-column?
  expect_true(inherits(sample_obs_nested$observations_total, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(sample_obs_nested$observations_total, inherits, "sf")))

  # Test with nested is FALSE
  sample_obs_unnested <- map_sample_observations(df = sim_occ3, nested = FALSE)

  # Is the observations_total column removed?
  expect_false("observations_total" %in% colnames(sample_obs_unnested))
  # Do we have unnested successfully?
  expect_true(nrow(sample_obs_unnested) > nrow(sample_obs_nested))
  sample_obs_unnested_test <- tidyr::unnest(
    sample_obs_nested,
    cols = "observations_total",
    names_repair = "minimal")
  sample_obs_unnested_test <- sample_obs_unnested_test[
    , !duplicated(t(sample_obs_unnested_test))
  ]
  expect_equal(sample_obs_unnested_test, sample_obs_unnested)
})

test_that("map_sample_observations handles invalid inputs", {
  # Invalid dataframe input
  expect_error(map_sample_observations(df = list(), nested = TRUE),
               "`df` must be a dataframe.")

  # Invalid nested argument
  expect_error(map_sample_observations(df = sim_occ1,
                                       nested = "TRUE"),
               "`nested` must be a logical vector of length 1.")

  # Invalid arg_list
  invalid_arg_list <- list(
      plgn = "polygon",
      sd_step = "sd",
      detection_probability = TRUE
    )
  expect_error(
    map_sample_observations(df = sim_occ2,
                            arg_list = invalid_arg_list),
    "`arg_list` must be named list containing one string for each value."
  )

  invalid_arg_list2 <- list(
      plgn = "polygon",
      sd_step = "sd",
      detection_probability = "det_probab"
    )
  expect_error(
    map_simulate_occurrences(df = sim_occ2,
                             arg_list = invalid_arg_list2),
    "You have provided column names in `arg_list` that are not present in `df`."
  )
})
