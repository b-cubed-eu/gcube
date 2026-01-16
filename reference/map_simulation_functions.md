# Map a cube simulation function over multiple rows of a dataframe

This function executes a cube simulation function
([`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md),
[`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md),
[`filter_observations()`](https://b-cubed-eu.github.io/gcube/reference/filter_observations.md),
[`add_coordinate_uncertainty()`](https://b-cubed-eu.github.io/gcube/reference/add_coordinate_uncertainty.md),
or
[`grid_designation()`](https://b-cubed-eu.github.io/gcube/reference/grid_designation.md))
over multiple rows of a dataframe with potentially different function
arguments over multiple columns.

## Usage

``` r
map_simulation_functions(f, df, nested = TRUE, progress = FALSE)
```

## Arguments

- f:

  One of five cube simulation functions:
  [`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md),
  [`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md),
  [`filter_observations()`](https://b-cubed-eu.github.io/gcube/reference/filter_observations.md),
  [`add_coordinate_uncertainty()`](https://b-cubed-eu.github.io/gcube/reference/add_coordinate_uncertainty.md),
  or
  [`grid_designation()`](https://b-cubed-eu.github.io/gcube/reference/grid_designation.md).

- df:

  A dataframe containing multiple rows, each representing a different
  species. The columns are function arguments with values used for
  mapping `f` for each species. Columns not used by this function will
  be retained in the output.

- nested:

  Logical. If `TRUE` (default), retains list-column containing
  dataframes calculated by `f`. Otherwise, expands this list-column into
  rows and columns.

- progress:

  Logical. Whether to show a progress bar. Set to `TRUE` to display a
  progress bar, `FALSE` (default) to suppress it.

## Value

In case of `nested = TRUE`, a dataframe identical to `df`, with an extra
list-column called `mapped_col` containing an sf object for each row
computed by the function specified in `f`. In case of `nested = FALSE`,
this list-column is expanded into additional rows and columns.

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
  initial_average_occurrences = c(50, 100, 500),
  n_time_points = rep(6, 3),
  temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
  sd_step = c(1, 1, NA),
  spatial_pattern = "random",
  seed = 123)

# Simulate occurrences
sim_occ_raw <- map_simulation_functions(
  f = simulate_occurrences,
  df = species_dataset_df)
sim_occ_raw

# Unnest output and create sf object
sim_occ_raw_unnested <- map_simulation_functions(
  f = simulate_occurrences,
  df = species_dataset_df,
  nested = FALSE)

sim_occ_raw_unnested %>%
   st_sf()
} # }
```
