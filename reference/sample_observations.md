# Sample observations from a larger occurrence dataset

The function computes observations from occurrences based on detection
probability and sampling bias by implementing a Bernoulli trial.

## Usage

``` r
sample_observations(
  occurrences,
  detection_probability = 1,
  sampling_bias = c("no_bias", "polygon", "manual"),
  bias_area = NA,
  bias_strength = 1,
  bias_weights = NA,
  seed = NA
)
```

## Arguments

- occurrences:

  An sf object with POINT geometry representing the occurrences.

- detection_probability:

  A numeric value between 0 and 1 representing the probability of
  detecting the species.

- sampling_bias:

  A character string specifying the method to generate a sampling bias.
  Options are `"no_bias"`, `"polygon"`, or `"manual"`.

  `"no_bias"`

  :   No bias is applied (default).

  `"polygon"`

  :   Bias the sampling within a polygon. Provide the polygon to
      `bias_area` and the bias strength to `bias_strength`.

  `"manual"`

  :   Bias the sampling manually using a grid. Provide the grid layer in
      which each cell contains the probability of being sampled to
      `bias_weights`.

- bias_area:

  An `sf` object with POLYGON geometry, or `NA`. Only used if
  `sampling_bias = "polygon"`. This defines the area in which the
  sampling will be biased.

- bias_strength:

  A positive numeric value, or `NA`. Only used if
  `sampling_bias = "polygon"`. The value represents the strength of the
  bias to be applied within the `bias_area`. Values greater than 1 will
  increase the sampling probability within the polygon relative to
  outside (oversampling), while values between 0 and 1 will decrease it
  (undersampling). For instance, a value of 50 will make the probability
  50 times higher within the `bias_area` compared to outside, whereas a
  value of 0.5 will make it half as likely.

- bias_weights:

  A grid layer (an sf object with POLYGON geometry), or `NA`. Only used
  if `sampling_bias = "manual"`. The grid of bias weights to be applied.
  This sf object should contain a `bias_weight` column with the weights
  per grid cell. Higher weights increase the probability of sampling.
  Weights can be numeric values between 0 and 1 or positive integers,
  which will be rescaled to values between 0 and 1.

- seed:

  A positive numeric value setting the seed for random number generation
  to ensure reproducibility. If `NA` (default), then
  [`set.seed()`](https://rdrr.io/r/base/Random.html) is not called at
  all. If not `NA`, then the random number generator state is reset (to
  the state before calling this function) upon exiting this function.

## Value

An sf object with POINT geometry containing the locations of the
occurrence with detection status. The object includes the following
columns:

- `detection_probability`:

  The detection probability for each occurrence (will be the same for
  all).

- `bias_weight`:

  The sampling probability based on sampling bias for each occurrence.

- `sampling_probability`:

  The combined sampling probability from detection probability and
  sampling bias for each occurrence.

- `observed`:

  Indicates whether the occurrence was detected (`TRUE`) or not
  (`FALSE`). Detected occurrences are called observations.

## See also

Other main:
[`add_coordinate_uncertainty()`](https://b-cubed-eu.github.io/gcube/reference/add_coordinate_uncertainty.md),
[`filter_observations()`](https://b-cubed-eu.github.io/gcube/reference/filter_observations.md),
[`grid_designation()`](https://b-cubed-eu.github.io/gcube/reference/grid_designation.md),
[`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md),
[`virtualsample_to_sf()`](https://b-cubed-eu.github.io/gcube/reference/virtualsample_to_sf.md)

## Examples

``` r
# Load packages
library(sf)
library(dplyr)

# Simulate some occurrence data with coordinates and time points
num_points <- 10
occurrences <- data.frame(
  lon = runif(num_points, min = -180, max = 180),
  lat = runif(num_points, min = -90, max = 90),
  time_point = 0
  )

# Convert the occurrence data to an sf object
occurrences_sf <- st_as_sf(occurrences, coords = c("lon", "lat"))

# 1. Sample observations without sampling bias
sample_observations(
  occurrences_sf,
  detection_probability = 0.8,
  sampling_bias = "no_bias",
  seed = 123
  )
#> Simple feature collection with 10 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -176.3391 ymin: -62.5651 xmax: 119.6027 ymax: 85.49863
#> CRS:           NA
#> # A tibble: 10 × 6
#>    time_point detection_probability bias_weight sampling_probability observed
#>         <dbl>                 <dbl>       <dbl>                <dbl> <lgl>   
#>  1          0                   0.8           1                  0.8 TRUE    
#>  2          0                   0.8           1                  0.8 TRUE    
#>  3          0                   0.8           1                  0.8 TRUE    
#>  4          0                   0.8           1                  0.8 FALSE   
#>  5          0                   0.8           1                  0.8 FALSE   
#>  6          0                   0.8           1                  0.8 TRUE    
#>  7          0                   0.8           1                  0.8 TRUE    
#>  8          0                   0.8           1                  0.8 FALSE   
#>  9          0                   0.8           1                  0.8 TRUE    
#> 10          0                   0.8           1                  0.8 TRUE    
#> # ℹ 1 more variable: geometry <POINT>

# 2. Sample observations with sampling bias in a polygon
# Create bias_area polygon overlapping two of the points
selected_observations <- st_union(occurrences_sf[2:3,])
bias_area <- st_convex_hull(selected_observations) %>%
  st_buffer(dist = 50) %>%
  st_as_sf()

sample_observations(
  occurrences_sf,
  detection_probability = 0.8,
  sampling_bias = "polygon",
  bias_area = bias_area,
  bias_strength = 2,
  seed = 123
  )
#> Simple feature collection with 10 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -176.3391 ymin: -62.5651 xmax: 119.6027 ymax: 85.49863
#> CRS:           NA
#> # A tibble: 10 × 6
#>    time_point detection_probability bias_weight sampling_probability observed
#>         <dbl>                 <dbl>       <dbl>                <dbl> <lgl>   
#>  1          0                   0.8       0.333                0.267 FALSE   
#>  2          0                   0.8       0.667                0.533 FALSE   
#>  3          0                   0.8       0.667                0.533 TRUE    
#>  4          0                   0.8       0.333                0.267 TRUE    
#>  5          0                   0.8       0.333                0.267 TRUE    
#>  6          0                   0.8       0.333                0.267 FALSE   
#>  7          0                   0.8       0.333                0.267 FALSE   
#>  8          0                   0.8       0.333                0.267 TRUE    
#>  9          0                   0.8       0.333                0.267 FALSE   
#> 10          0                   0.8       0.667                0.533 TRUE    
#> # ℹ 1 more variable: geometry <POINT>

# 3. Sample observations with sampling bias given manually in a grid
# Create raster grid with bias weights between 0 and 1
grid <- st_make_grid(occurrences_sf) %>%
  st_sf() %>%
  mutate(bias_weight = runif(n(), min = 0, max = 1))

sample_observations(
  occurrences_sf,
  detection_probability = 0.8,
  sampling_bias = "manual",
  bias_weights = grid,
  seed = 123
  )
#> Simple feature collection with 10 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -176.3391 ymin: -62.5651 xmax: 119.6027 ymax: 85.49863
#> CRS:           NA
#> # A tibble: 10 × 6
#>    time_point detection_probability bias_weight sampling_probability observed
#>         <dbl>                 <dbl>       <dbl>                <dbl> <lgl>   
#>  1          0                   0.8       0.967                0.774 TRUE    
#>  2          0                   0.8       0.371                0.296 TRUE    
#>  3          0                   0.8       0.183                0.147 FALSE   
#>  4          0                   0.8       0.218                0.174 TRUE    
#>  5          0                   0.8       0.287                0.230 TRUE    
#>  6          0                   0.8       0.733                0.586 TRUE    
#>  7          0                   0.8       0.909                0.727 TRUE    
#>  8          0                   0.8       0.192                0.153 TRUE    
#>  9          0                   0.8       0.445                0.356 FALSE   
#> 10          0                   0.8       0.279                0.223 FALSE   
#> # ℹ 1 more variable: geometry <POINT>
```
