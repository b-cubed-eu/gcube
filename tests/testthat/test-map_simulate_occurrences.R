## Prepare example datasets
# Create polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

# Specify dataframe for 3 species with custom function arguments
# Dataframe with column names equal to arguments
species_dataset_df1 <- tibble(
  taxonID = c("species1", "species2", "species3"),
  plgn = rep(list(plgn), 3),
  initial_average_abundance = c(50, 100, 500),
  n_time_points = rep(6, 3),
  temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
  sd_step = c(1, 1, NA),
  spatial_autocorr = "random",
  seed = 123)

# Dataframe with custom column names and named list for argument conversion
species_dataset_df2 <- species_dataset_df1 %>%
  rename(polygon = plgn,
         sd = sd_step)


## Unit tests

test_that("map_simulate_occurrences works with simple column names", {
  # Test with nested is TRUE
  sim_occ_nested <- map_simulate_occurrences(df = species_dataset_df1)

  # Are previous column names retained and one extra column name created?
  expect_true("occurrences" %in% colnames(sim_occ_nested))
  expect_equal(sort(c(colnames(species_dataset_df1), "occurrences")),
               sort(colnames(sim_occ_nested)))
  # Is the new column a list-column?
  expect_true(inherits(sim_occ_nested$occurrences, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(sim_occ_nested$occurrences, inherits, "sf")))

  # Test with nested is FALSE
  sim_occ_unnested <- map_simulate_occurrences(df = species_dataset_df1,
                                               nested = FALSE)

  # Is the occurrence column removed?
  expect_false("occurrences" %in% colnames(sim_occ_unnested))
  # Do we have unnested successfully?
  expect_true(nrow(sim_occ_unnested) > nrow(species_dataset_df1))
  expect_equal(tidyr::unnest(sim_occ_nested, "occurrences"),
               sim_occ_unnested)
})

test_that("map_simulate_occurrences works with arg_list for renaming columns", {
  # Test with arg_list
  arg_conv_list <- list(
      plgn = "polygon",
      sd_step = "sd"
    )

  sim_occ_nested <- map_simulate_occurrences(df = species_dataset_df2,
                                             arg_list = arg_conv_list)

  # Are previous column names retained and one extra column name created?
  expect_true("occurrences" %in% colnames(sim_occ_nested))
  expect_equal(sort(c(colnames(species_dataset_df2), "occurrences")),
               sort(colnames(sim_occ_nested)))
  # Is the new column a list-column?
  expect_true(inherits(sim_occ_nested$occurrences, "list"))
  # Is the output of the function an sf object for each species (each row)?
  expect_true(all(sapply(sim_occ_nested$occurrences, inherits, "sf")))
})

test_that("map_simulate_occurrences handles invalid inputs", {
  # Invalid dataframe input
  expect_error(map_simulate_occurrences(df = list(), nested = TRUE),
               "`df` must be a dataframe.")

  # Invalid nested argument
  expect_error(map_simulate_occurrences(df = species_dataset_df1,
                                        nested = "TRUE"),
               "`nested` must be a logical vector of length 1.")

  # Invalid arg_list
  invalid_arg_list <- list(
      plgn = "polygon",
      sd_step = 123
    )
  expect_error(
    map_simulate_occurrences(df = species_dataset_df2,
                             arg_list = invalid_arg_list),
    "`arg_list` must be named list containing one string for each value."
  )

  invalid_arg_list2 <- list(
      plgn = "polygon",
      sd_step = "sd",
      detection_probability = "det_prob"
    )
  expect_error(
    map_simulate_occurrences(df = species_dataset_df2,
                             arg_list = invalid_arg_list2),
    "You have provided column names in `arg_list` that are not present in `df`."
  )
})
