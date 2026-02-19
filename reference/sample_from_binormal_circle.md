# Sample from a circle using the bivariate Normal distribution

This function samples a new observations point of a species within the
uncertainty circle around each observation assuming a bivariate Normal
distribution.

## Usage

``` r
sample_from_binormal_circle(
  observations,
  p_norm = 0.95,
  missing_uncertainty = 1000,
  seed = NA
)
```

## Arguments

- observations:

  An sf object with POINT geometry and a `time_point` and
  `coordinateUncertaintyInMeters` column. If the former column is not
  present, the function will assume a single time point. If the latter
  column is not present, the function will assume no uncertainty (zero
  meters) around the observation points.

- p_norm:

  A numeric value between 0 and 1. The proportion of all possible
  samples from a bivariate Normal distribution that fall within the
  uncertainty circle. Default is 0.95. See Details.

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

## Value

An sf object with POINT geometry containing the locations of the sampled
occurrences and a `coordinateUncertaintyInMeters` column containing the
coordinate uncertainty for each observation.

## Details

A new observation point is sampled from a bivariate Normal distribution
with means equal to the X and Y coordinates of its original observation
point and variances equal to (-`coordinateUncertaintyInMeters`^2) / (2
\* log(1 - `p_norm`)), ensuring `p_norm` % of all possible samples fall
within the uncertainty circle.

## See also

Other designation:
[`sample_from_uniform_circle()`](https://b-cubed-eu.github.io/gcube/reference/sample_from_uniform_circle.md)

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

# Sample points within uncertainty circles according to normal rules
sample_from_binormal_circle(
  observations = observations_sf,
  p_norm = 0.95,
  seed = 123
)
#> Simple feature collection with 4 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 3841069 ymin: 3110889 xmax: 3841328 ymax: 3111675
#> Projected CRS: ETRS89-extended / LAEA Europe
#>   time_point coordinateUncertaintyInMeters                geometry
#> 1          1                      68.90797 POINT (3841319 3110889)
#> 2          1                      29.01942 POINT (3841069 3111675)
#> 3          1                      20.95363 POINT (3841201 3111458)
#> 4          1                      81.30416 POINT (3841328 3111493)
```
