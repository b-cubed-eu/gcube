## Prepare example datasets
# Create a simple polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

# Create grid
cube_grid <- st_make_grid(
    st_buffer(plgn, 50),
    n = c(20, 20),
    square = TRUE) %>%
  st_sf()

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
  grid = rep(list(cube_grid), 3),
  seed = 123)

# Dataframe with custom column names and named list for argument conversion for
# simple polygon. Create named list for argument conversion.
species_dataset_df2 <- species_dataset_df1 %>%
  rename(polygon = species_range,
         sd = sd_step,
         det_prob = detection_probability,
         inv = invert,
         coord_uncertainty = coords_uncertainty_meters,
         raster = grid)

arg_conv_list <- list(
    species_range = "polygon",
    sd_step = "sd",
    detection_probability = "det_prob",
    invert = "inv",
    coords_uncertainty_meters = "coord_uncertainty",
    grid = "raster"
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

# Add coordinate uncertainty
obs_uncertainty1 <- map_add_coordinate_uncertainty(
  df = filter_obs1)
obs_uncertainty2 <- map_add_coordinate_uncertainty(
  df = filter_obs2,
  arg_list = arg_conv_list)


## Unit tests

test_that("map_grid_designation works with simple column names", {
  # Test with nested is TRUE
  occ_cube_nested <- map_grid_designation(df = obs_uncertainty1)

  # Are previous column names retained and one extra column name created?
  expect_true("occurrence_cube_df" %in% colnames(occ_cube_nested))
  expect_equal(sort(c(colnames(obs_uncertainty1), "occurrence_cube_df")),
               sort(colnames(occ_cube_nested)))
  # Is the new column a list-column?
  expect_true(inherits(occ_cube_nested$occurrence_cube_df, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(occ_cube_nested$occurrence_cube_df, inherits, "sf")))

  # Test with nested is FALSE
  occ_cube_unnested <- map_grid_designation(df = obs_uncertainty1,
                                            nested = FALSE)

  # Is the occurrence_cube_df column removed?
  expect_false("occurrence_cube_df" %in% colnames(occ_cube_unnested))
  # Do we have unnested successfully?
  expect_true(nrow(occ_cube_unnested) > nrow(occ_cube_nested))
  occ_cube_unnested_test <- tidyr::unnest(
    occ_cube_nested,
    cols = "occurrence_cube_df",
    names_repair = "minimal")
  occ_cube_unnested_test <- occ_cube_unnested_test[
    , !duplicated(t(occ_cube_unnested_test))
  ]
  expect_equal(occ_cube_unnested_test, occ_cube_unnested)
})

test_that("map_grid_designation works with pipes", {
  occ_cube_piped <- tibble(
      species = c("species1", "species2", "species3"),
      species_range = rep(list(plgn), 3),
      grid = rep(list(cube_grid), 3),
      seed = 123
    ) %>%
    map_simulate_occurrences() %>%
    map_sample_observations() %>%
    map_filter_observations() %>%
    map_add_coordinate_uncertainty() %>%
    map_grid_designation(nested = FALSE)

  # Is the occurrence_cube_df column removed?
  expect_false("occurrence_cube_df" %in% colnames(occ_cube_piped))
  # Do we have the expected columns?
  expect_equal(
    sort(colnames(occ_cube_piped)),
    sort(c("species", "species_range", "grid", "seed", "occurrences",
           "observations_total", "observations", "time_point", "id", "n",
           "min_coord_uncertainty", "geometry")))
})

test_that("map_grid_designation works with arg_list for renaming columns", {
  # Test with arg_list
  occ_cube_nested <- map_grid_designation(df = obs_uncertainty2,
                                          arg_list = arg_conv_list)

  # Are previous column names retained and one extra column name created?
  expect_true("occurrence_cube_df" %in% colnames(occ_cube_nested))
  expect_equal(sort(c(colnames(obs_uncertainty2), "occurrence_cube_df")),
               sort(colnames(occ_cube_nested)))
  # Is the new column a list-column?
  expect_true(inherits(occ_cube_nested$occurrence_cube_df, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(occ_cube_nested$occurrence_cube_df, inherits, "sf")))
})

test_that("map_grid_designation without coordinate uncertainty", {
  # Test with nested is TRUE
  suppressWarnings({
    occ_cube_nested <- map_grid_designation(df = filter_obs1)
  })

  # Test warning handling
  w <- testthat::capture_warnings(map_grid_designation(df = filter_obs1))
  expect_match(w[1], "3 warnings during mapping:", all = FALSE)
  expect_match(
    w[2],
    "No column `coordinateUncertaintyInMeters` present!.+\\[3 times\\]",
    all = FALSE)


  # Are previous column names retained and one extra column name created?
  expect_true("occurrence_cube_df" %in% colnames(occ_cube_nested))
  expect_equal(sort(c(colnames(filter_obs1), "occurrence_cube_df")),
               sort(colnames(occ_cube_nested)))
  # Is the new column a list-column?
  expect_true(inherits(occ_cube_nested$occurrence_cube_df, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(occ_cube_nested$occurrence_cube_df, inherits, "sf")))

  # Test with nested is FALSE
  suppressWarnings({
  occ_cube_unnested <- map_grid_designation(df = filter_obs1,
                                            nested = FALSE)
  })

  # Test warning handling
  w_unnested <- testthat::capture_warnings(
    map_grid_designation(df = filter_obs1,
                         nested = FALSE))
  expect_match(w_unnested[1], "3 warnings during mapping:", all = FALSE)
  expect_match(
      w_unnested[2],
      "No column `coordinateUncertaintyInMeters` present!.+\\[3 times\\]",
      all = FALSE)

  # Is the occurrence_cube_df column removed?
  expect_false("occurrence_cube_df" %in% colnames(occ_cube_unnested))
  # Do we have unnested successfully?
  expect_true(nrow(occ_cube_unnested) > nrow(occ_cube_nested))
  occ_cube_unnested_test <- tidyr::unnest(
    occ_cube_nested,
    cols = "occurrence_cube_df",
    names_repair = "minimal")
  occ_cube_unnested_test <- occ_cube_unnested_test[
    , !duplicated(t(occ_cube_unnested_test))
  ]
  expect_equal(occ_cube_unnested_test, occ_cube_unnested)
})

test_that("map_grid_designation handles invalid inputs", {
  # Invalid dataframe input
  expect_error(map_grid_designation(df = list(), nested = TRUE),
               "`df` must be a dataframe.")

  # Invalid nested argument
  expect_error(map_grid_designation(df = sim_occ1,
                                    nested = "TRUE"),
               "`nested` must be a logical vector of length 1.")

  # Invalid arg_list
  invalid_arg_list <- arg_conv_list <- list(
    species_range = "polygon",
    sd_step = 123,
    detection_probability = "det_prob",
    invert = "inv",
    coords_uncertainty_meters = "coord_uncertainty",
    grid = "raster"
  )
  expect_error(
    map_grid_designation(df = sim_occ2,
                         arg_list = invalid_arg_list),
    "`arg_list` must be named list containing one string for each value."
  )

  invalid_arg_list2 <- arg_conv_list <- list(
    species_range = "polygon",
    sd_step = "sd",
    detection_probability = "det_prob",
    invert = "inv",
    coords_uncertainty_meters = "coord_uncertainty",
    grid = "rasters"
  )
  expect_error(
    map_simulate_occurrences(df = sim_occ2,
                             arg_list = invalid_arg_list2),
    "You have provided column names in `arg_list` that are not present in `df`."
  )
})
