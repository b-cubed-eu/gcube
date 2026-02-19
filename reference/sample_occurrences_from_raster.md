# Sample occurrences from spatial random field

This function draws point occurrences from a spatial random field
represented by a raster. Points are sampled based on the values in the
raster, with the number of occurrences specified for each time step.

## Usage

``` r
sample_occurrences_from_raster(raster, time_series, seed = NA)
```

## Arguments

- raster:

  A SpatRaster object (see
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)).

- time_series:

  A vector with the number of occurrences per time point.

- seed:

  A positive numeric value setting the seed for random number generation
  to ensure reproducibility. If `NA` (default), then
  [`set.seed()`](https://rdrr.io/r/base/Random.html) is not called at
  all. If not `NA`, then the random number generator state is reset (to
  the state before calling this function) upon exiting this function.

## Value

An sf object with POINT geometry containing the locations of the
simulated occurrences, a `time_point` column indicating the associated
time point for each occurrence and columns used as weights for sampling.
If the raster is created with
[`create_spatial_pattern()`](https://b-cubed-eu.github.io/gcube/reference/create_spatial_pattern.md),
the column `sampling_p1` is used.

## See also

Other occurrence:
[`create_spatial_pattern()`](https://b-cubed-eu.github.io/gcube/reference/create_spatial_pattern.md),
[`simulate_random_walk()`](https://b-cubed-eu.github.io/gcube/reference/simulate_random_walk.md),
[`simulate_timeseries()`](https://b-cubed-eu.github.io/gcube/reference/simulate_timeseries.md)

## Examples

``` r
# Load packages
library(sf)
library(ggplot2)
library(tidyterra)

# Create polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

## Medium scale clustering
# Create the random field
rs_pattern_clustered <- create_spatial_pattern(
  polygon = plgn,
  resolution = 0.1,
  spatial_pattern = "clustered",
  seed = 123)
#> [using unconditional Gaussian simulation]

# Sample 200 occurrences from random field
pts_occ_clustered <- sample_occurrences_from_raster(
  raster = rs_pattern_clustered,
  time_series = 200,
  seed = 123)

ggplot() +
  geom_spatraster(data = rs_pattern_clustered) +
  geom_sf(data = pts_occ_clustered) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()


## Large scale clustering
# Create the random field
rs_pattern_large <- create_spatial_pattern(
  polygon = plgn,
  resolution = 0.1,
  spatial_pattern = 100,
  seed = 123)
#> [using unconditional Gaussian simulation]

# Sample 200 occurrences from random field
pts_occ_large <- sample_occurrences_from_raster(
  raster = rs_pattern_large,
  time_series = 200,
  seed = 123)

ggplot() +
  geom_spatraster(data = rs_pattern_large) +
  geom_sf(data = pts_occ_large) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()
```
