# Observations to grid designation to create a data cube

This function designates observations to cells of a given grid to create
an aggregated data cube.

## Usage

``` r
grid_designation(
  observations,
  grid,
  id_col = "row_names",
  missing_uncertainty = 1000,
  seed = NA,
  aggregate = TRUE,
  randomisation = c("uniform", "normal"),
  p_norm = ifelse(tolower(randomisation[1]) == "uniform", NA, 0.95)
)
```

## Arguments

- observations:

  An sf object with POINT geometry and a `time_point` and
  `coordinateUncertaintyInMeters` column. If the former column is not
  present, the function will assume a single time point. If the latter
  column is not present, the function will assume no uncertainty (zero
  meters) around the observation points.

- grid:

  An sf object with POLYGON geometry (usually a grid) to which
  observations should be designated.

- id_col:

  The column name containing unique IDs for each grid cell. If
  `"row_names"` (the default), a new column `cell_code` is created where
  the row names represent the unique IDs.

- missing_uncertainty:

  A positive numeric value (default: 1000 m) used to replace missing
  (`NA`) values in the `coordinateUncertaintyInMeters` column. This
  ensures that all observations have a defined uncertainty radius for
  sampling. Only applied when the column is present but contains `NA`
  values; if the column itself is absent, a value of 0 is assumed
  instead.

- seed:

  A positive numeric value setting the seed for random number generation
  to ensure reproducibility. If `NA` (default), then
  [`set.seed()`](https://rdrr.io/r/base/Random.html) is not called at
  all. If not `NA`, then the random number generator state is reset (to
  the state before calling this function) upon exiting this function.

- aggregate:

  Logical. If `TRUE` (default), returns data cube in aggregated form
  (grid with the number of observations per grid cell). Otherwise,
  returns sampled points within the uncertainty circle.

- randomisation:

  Character. Method used for sampling within the uncertainty circle
  around each observation. `"uniform"` (default) means each point in the
  uncertainty circle has an equal probability of being selected. The
  other option is `"normal"`, where a point is sampled from a bivariate
  Normal distribution with means equal to the observation point and
  variance such that `p_norm` % of all possible samples from this Normal
  distribution fall within the uncertainty circle. See
  [`sample_from_binormal_circle()`](https://b-cubed-eu.github.io/gcube/reference/sample_from_binormal_circle.md).

- p_norm:

  A numeric value between 0 and 1, used only if
  `randomisation = "normal"`. The proportion of all possible samples
  from a bivariate Normal distribution that fall within the uncertainty
  circle. Default is 0.95.

## Value

If `aggregate = TRUE`, an sf object with POLYGON geometry containing the
grid cells, an `n` column with the number of observations per grid cell,
and a `min_coord_uncertainty` column with the minimum coordinate
uncertainty per grid cell. If `aggregate = FALSE`, an sf object with
POINT geometry containing the sampled observations within the
uncertainty circles, and a `coordinateUncertaintyInMeters` column with
the coordinate uncertainty for each observation.

## See also

Other main:
[`add_coordinate_uncertainty()`](https://b-cubed-eu.github.io/gcube/reference/add_coordinate_uncertainty.md),
[`filter_observations()`](https://b-cubed-eu.github.io/gcube/reference/filter_observations.md),
[`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md),
[`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md),
[`virtualsample_to_sf()`](https://b-cubed-eu.github.io/gcube/reference/virtualsample_to_sf.md)

## Examples

``` r
library(sf)
library(dplyr)

# Create four random points
n_points <- 4
xlim <- c(3841000, 3842000)
ylim <- c(3110000, 3112000)
coordinate_uncertainty <- rgamma(n_points, shape = 5, rate = 0.1)

observations_sf <- data.frame(
  lat = runif(n_points, ylim[1], ylim[2]),
  long = runif(n_points, xlim[1], xlim[2]),
  time_point = 1,
  coordinateUncertaintyInMeters = coordinate_uncertainty
) %>%
  st_as_sf(coords = c("long", "lat"), crs = 3035)

# Add buffer uncertainty in meters around points
observations_buffered <- observations_sf %>%
  st_buffer(observations_sf$coordinateUncertaintyInMeters)

# Create grid
grid_df <- st_make_grid(
  observations_buffered,
  square = TRUE,
  cellsize = c(200, 200)
) %>%
  st_sf()

# Create occurrence cube
grid_designation(
  observations = observations_sf,
  grid = grid_df,
  seed = 123
)
#> Simple feature collection with 32 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 3841035 ymin: 3110281 xmax: 3841835 ymax: 3111881
#> Projected CRS: ETRS89-extended / LAEA Europe
#> # A tibble: 32 × 5
#>    time_point cell_code     n min_coord_uncertainty                     geometry
#>  *      <dbl> <chr>     <int>                 <dbl>                <POLYGON [m]>
#>  1          1 26            1                  40.3 ((3841235 3111481, 3841435 …
#>  2          1 29            1                  28.6 ((3841035 3111681, 3841235 …
#>  3          1 3             1                  69.5 ((3841435 3110281, 3841635 …
#>  4          1 4             1                  65.0 ((3841635 3110281, 3841835 …
#>  5          1 1             0                  NA   ((3841035 3110281, 3841235 …
#>  6          1 2             0                  NA   ((3841235 3110281, 3841435 …
#>  7          1 5             0                  NA   ((3841035 3110481, 3841235 …
#>  8          1 6             0                  NA   ((3841235 3110481, 3841435 …
#>  9          1 7             0                  NA   ((3841435 3110481, 3841635 …
#> 10          1 8             0                  NA   ((3841635 3110481, 3841835 …
#> # ℹ 22 more rows
```
