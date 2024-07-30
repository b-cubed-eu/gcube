## Prepare example datasets
# Create a simple polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

# Specify dataframe for 3 species with custom function arguments
# Dataframe with column names equal to arguments for simple polygon
species_dataset_df1 <- tibble(
  taxonID = c("species1", "species2", "species3"),
  plgn = rep(list(plgn), 3),
  initial_average_occurrences = c(50, 100, 500),
  n_time_points = rep(6, 3),
  temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
  sd_step = c(1, 1, NA),
  spatial_autocorr = "random",
  detection_probability = c(0.8, 0.9, 1),
  invert = FALSE,
  seed = 123)

# Dataframe with custom column names and named list for argument conversion for
# simple polygon. Create named list for argument conversion.
species_dataset_df2 <- species_dataset_df1 %>%
  rename(polygon = plgn,
         sd = sd_step,
         det_prob = detection_probability,
         inv = invert)

arg_conv_list <- list(
    plgn = "polygon",
    sd_step = "sd",
    detection_probability = "det_prob",
    invert = "inv"
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


## Unit tests

test_that("map_filter_observations works with simple column names", {
  # Test with nested is TRUE
  filter_obs_nested <- map_filter_observations(df = samp_obs1)

  # Are previous column names retained and one extra column name created?
  expect_true("observations" %in% colnames(filter_obs_nested))
  expect_equal(sort(c(colnames(samp_obs1), "observations")),
               sort(colnames(filter_obs_nested)))
  # Is the new column a list-column?
  expect_true(inherits(filter_obs_nested$observations, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(filter_obs_nested$observations, inherits, "sf")))

  # Test with nested is FALSE
  filter_obs_unnested <- map_filter_observations(df = samp_obs1, nested = FALSE)

  # Is the observations column removed?
  expect_false("observations" %in% colnames(filter_obs_unnested))
  # Do we have unnested successfully?
  expect_true(nrow(filter_obs_unnested) > nrow(filter_obs_nested))
  filter_obs_unnested_test <- tidyr::unnest(
    filter_obs_nested,
    cols = "observations",
    names_repair = "minimal")
  filter_obs_unnested_test <- filter_obs_unnested_test[
    , !duplicated(t(filter_obs_unnested_test))
  ]
  expect_equal(filter_obs_unnested_test, filter_obs_unnested)
})

test_that("map_filter_observations works with arg_list for renaming columns", {
  # Test with arg_list
  filter_obs_nested <- map_filter_observations(df = samp_obs2,
                                               arg_list = arg_conv_list)

  # Are previous column names retained and one extra column name created?
  expect_true("observations" %in% colnames(filter_obs_nested))
  expect_equal(sort(c(colnames(samp_obs2), "observations")),
               sort(colnames(filter_obs_nested)))
  # Is the new column a list-column?
  expect_true(inherits(filter_obs_nested$observations, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(filter_obs_nested$observations, inherits, "sf")))
})

test_that("map_filter_observations handles invalid inputs", {
  # Invalid dataframe input
  expect_error(map_filter_observations(df = list(), nested = TRUE),
               "`df` must be a dataframe.")

  # Invalid nested argument
  expect_error(map_filter_observations(df = sim_occ1,
                                       nested = "TRUE"),
               "`nested` must be a logical vector of length 1.")

  # Invalid arg_list
  invalid_arg_list <- list(
      plgn = "polygon",
      sd_step = "sd",
      detection_probability = "det_prob",
      invert = 1
    )
  expect_error(
    map_filter_observations(df = sim_occ2,
                            arg_list = invalid_arg_list),
    "`arg_list` must be named list containing one string for each value."
  )

  invalid_arg_list2 <- list(
      plgn = "polygon",
      sd_step = "sd",
      detection_probability = "det_prob",
      invert = "invert_col"
    )
  expect_error(
    map_simulate_occurrences(df = sim_occ2,
                             arg_list = invalid_arg_list2),
    "You have provided column names in `arg_list` that are not present in `df`."
  )
})
