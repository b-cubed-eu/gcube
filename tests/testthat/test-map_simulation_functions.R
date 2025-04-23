## Prepare example datasets
# Create a simple polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

# Create grid
cube_grid <- st_make_grid(
  st_buffer(plgn, 25),
  n = c(20, 20),
  square = TRUE
) %>%
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
  spatial_autocorr = "random",
  detection_probability = c(0.8, 0.9, 1),
  invert = FALSE,
  coords_uncertainty_meters = c(25, 30, 50),
  grid = rep(list(cube_grid), 3),
  seed = 123
)

# Map simulate occurrences
sim_occ1 <- map_simulate_occurrences(
  df = species_dataset_df1
)
sim_occ1_unnested <- map_simulate_occurrences(
  df = species_dataset_df1,
  nested = FALSE
)

# Map sample observations
samp_obs1 <- map_sample_observations(
  df = sim_occ1
)
samp_obs1_unnested <- map_sample_observations(
  df = sim_occ1,
  nested = FALSE
)

# Map sample observations
filter_obs1 <- map_filter_observations(
  df = samp_obs1
)
filter_obs1_unnested <- map_filter_observations(
  df = samp_obs1,
  nested = FALSE
)

# Add coordinate uncertainty
obs_uncertainty1 <- map_add_coordinate_uncertainty(
  df = filter_obs1
)
obs_uncertainty1_unnested <- map_add_coordinate_uncertainty(
  df = filter_obs1,
  nested = FALSE
)

# Grid designation
occ_cube1 <- map_grid_designation(
  df = obs_uncertainty1
)
occ_cube1_unnested <- map_grid_designation(
  df = obs_uncertainty1,
  nested = FALSE
)


## Unit tests

test_that("map_simulation_functions works with map_simulate_occurrences", {
  # Test with nested is TRUE
  sim_occ1_test <- map_simulation_functions(
    f = simulate_occurrences,
    df = species_dataset_df1
  )

  expect_equal(sim_occ1,
               sim_occ1_test %>% rename("occurrences" = "mapped_col"))

  # Test with nested is FALSE
  sim_occ1_unnested_test <- map_simulation_functions(
    f = simulate_occurrences,
    df = species_dataset_df1,
    nested = FALSE
  )

  expect_equal(sim_occ1_unnested, sim_occ1_unnested_test)

  # Do we have unnested successfully?
  expect_true(nrow(sim_occ1_unnested_test) > nrow(sim_occ1_test))
})

test_that("map_simulation_functions works with map_sample_observations", {
  # Test with nested is TRUE
  samp_obs1_test <- map_simulation_functions(
    f = sample_observations,
    df = sim_occ1
  )

  expect_equal(samp_obs1,
               samp_obs1_test %>% rename("observations_total" = "mapped_col"))

  # Test with nested is FALSE
  samp_obs1_unnested_test <- map_simulation_functions(
    f = sample_observations,
    df = sim_occ1,
    nested = FALSE
  )

  expect_equal(samp_obs1_unnested, samp_obs1_unnested_test)

  # Do we have unnested successfully?
  expect_true(nrow(samp_obs1_unnested_test) > nrow(samp_obs1_test))
})

test_that("map_simulation_functions works with map_filter_observations", {
  # Test with nested is TRUE
  filter_obs1_test <- map_simulation_functions(
    f = filter_observations,
    df = samp_obs1
  )

  expect_equal(filter_obs1,
               filter_obs1_test %>% rename("observations" = "mapped_col"))

  # Test with nested is FALSE
  filter_obs1_unnested_test <- map_simulation_functions(
    f = filter_observations,
    df = samp_obs1,
    nested = FALSE
  )

  expect_equal(filter_obs1_unnested, filter_obs1_unnested_test)

  # Do we have unnested successfully?
  expect_true(nrow(filter_obs1_unnested_test) > nrow(filter_obs1_test))
})

test_that("map_simulation_functions works with coordinate uncertainty", {
  # Test with nested is TRUE
  filter_obs1_test <- map_simulation_functions(
    f = add_coordinate_uncertainty,
    df = filter_obs1
  )

  # Extra column?
  expect_equal(sort(c(colnames(filter_obs1$observations[[1]]),
                      "coordinateUncertaintyInMeters")),
               sort(colnames(filter_obs1_test$mapped_col[[1]])))

  # Test with nested is FALSE
  filter_obs1_unnested_test <- map_simulation_functions(
    f = add_coordinate_uncertainty,
    df = filter_obs1,
    nested = FALSE
  )
  expect_equal(filter_obs1_unnested,
               subset(filter_obs1_unnested_test, select = -observations))

  # Do we have unnested successfully?
  expect_true(nrow(filter_obs1_unnested_test) > nrow(filter_obs1_test))
})

test_that("map_simulation_functions works with map_grid_designation", {
  # Test with nested is TRUE
  occ_cube1_test <- map_simulation_functions(
    f = grid_designation,
    df = obs_uncertainty1
  )

  expect_equal(occ_cube1,
               occ_cube1_test %>% rename("occurrence_cube_df" = "mapped_col"))

  # Test with nested is FALSE
  occ_cube1_unnested_test <- map_simulation_functions(
    f = grid_designation,
    df = obs_uncertainty1,
    nested = FALSE
  )

  expect_equal(occ_cube1_unnested, occ_cube1_unnested_test)

  # Do we have unnested successfully?
  expect_true(nrow(occ_cube1_unnested_test) > nrow(occ_cube1_test))
})

test_that("map_simulation_functions handles invalid inputs", {
  # Invalid function input
  expect_error(
    map_simulation_functions(
      f = 123,
      df = species_dataset_df1,
      nested = TRUE
    ),
    "`f` must be a function.\nOne of.+, or `grid_designation()"
  )

  # Invalid function
  expect_error(
    map_simulation_functions(
      f = mean,
      df = species_dataset_df1,
      nested = TRUE
    ),
    "`f` must be a function.\nOne of.+, or `grid_designation()"
  )

  # Invalid dataframe input
  expect_error(
    map_simulation_functions(
      f = simulate_occurrences,
      df = list(),
      nested = TRUE
    ),
    "`df` must be a dataframe."
  )

  # Invalid nested argument
  expect_error(
    map_simulation_functions(
      f = simulate_occurrences,
      df = species_dataset_df1,
      nested = "TRUE"
    ),
    "`nested` must be a logical vector of length 1."
  )
})
