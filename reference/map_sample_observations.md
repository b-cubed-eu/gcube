# Map `sample_observations()` over multiple species

This function executes
[`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md)
over multiple rows of a dataframe, representing different species, with
potentially different function arguments over multiple columns.

## Usage

``` r
map_sample_observations(df, nested = TRUE, arg_list = NA, progress = FALSE)
```

## Arguments

- df:

  A dataframe containing multiple rows, each representing a different
  species. The columns are function arguments with values used for
  mapping
  [`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md)
  for each species. Columns not used by this function will be retained
  in the output.

- nested:

  Logical. If `TRUE` (default), retains list-column containing sf
  objects calculated by
  [`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md).
  Otherwise, expands this list-column into rows and columns.

- arg_list:

  A named list or `NA`. If `NA` (default), the function assumes column
  names in `df` are identical to argument names of
  [`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md).
  If column names differ, they must be specified as a named list where
  the names are the argument names of
  [`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md),
  and the associated values are the corresponding column names in `df`.

- progress:

  Logical. Whether to show a progress bar. Set to `TRUE` to display a
  progress bar, `FALSE` (default) to suppress it.

## Value

In case of `nested = TRUE`, a dataframe identical to `df`, with an extra
list-column called `occurrences` containing an sf object with POINT
geometry for each row computed by
[`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md).
In case of `nested = FALSE`, this list-column is expanded into
additional rows and columns.

## See also

Other multispecies:
[`generate_taxonomy()`](https://b-cubed-eu.github.io/gcube/reference/generate_taxonomy.md),
[`map_add_coordinate_uncertainty()`](https://b-cubed-eu.github.io/gcube/reference/map_add_coordinate_uncertainty.md),
[`map_filter_observations()`](https://b-cubed-eu.github.io/gcube/reference/map_filter_observations.md),
[`map_grid_designation()`](https://b-cubed-eu.github.io/gcube/reference/map_grid_designation.md),
[`map_simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/map_simulate_occurrences.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load packages
library(sf)
library(dplyr)

# Create polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

## Example with simple column names
# Specify dataframe for 3 species with custom function arguments
species_dataset_df <- tibble(
  taxonID = c("species1", "species2", "species3"),
  species_range = rep(list(plgn), 3),
  initial_average_occurrences = c(50, 100, 200),
  n_time_points = rep(6, 3),
  temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
  sd_step = c(1, 1, NA),
  spatial_pattern = "random",
  detection_probability = c(0.8, 0.9, 1),
  seed = 123)

# Simulate occurrences
sim_occ1 <- map_simulate_occurrences(df = species_dataset_df)

# Sample observations
samp_obs_nested <- map_sample_observations(df = sim_occ1)
samp_obs_nested

## Example with deviating column names
# Specify dataframe for 3 species with custom function arguments
species_dataset_df2 <- species_dataset_df %>%
  rename(polygon = species_range,
         sd = sd_step,
         det_prob = detection_probability)

# Create named list for argument conversion
arg_conv_list <- list(
    species_range = "polygon",
    sd_step = "sd",
    detection_probability = "det_prob"
  )

# Simulate occurrences
sim_occ2 <- map_simulate_occurrences(
  df = species_dataset_df2,
  arg_list = arg_conv_list)

# Sample observations
map_sample_observations(
  df = sim_occ2,
  arg_list = arg_conv_list)
} # }
```
