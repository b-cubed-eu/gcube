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
         sd = sd_step)

arg_conv_list <- list(
    plgn = "polygon",
    sd_step = "sd",
    detection_probability = "det_prob"
  )

# Dataframe with column names equal to arguments for road polygon
species_dataset_df1 <- tibble(
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
sim_occ_nested1 <- map_simulate_occurrences(
  df = species_dataset_df1)
sim_occ2 <- map_simulate_occurrences(
  df = species_dataset_df2,
  arg_list = arg_conv_list)
sim_occ3 <- map_simulate_occurrences(
  df = species_dataset_df3)

## Unit tests

