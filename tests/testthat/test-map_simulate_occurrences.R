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

arg_conv_list2 <- list(
    plgn = "polygon",
    sd_step = "sd"
  )

## Unit tests

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Do with and without named list and should be the same
# keep original column names
# nest = false same result if you nest afterwards
