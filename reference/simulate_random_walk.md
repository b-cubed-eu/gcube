# Simulate a random walk over time

This function simulates a timeseries for the average number of
occurrences of a species using a random walk over time.

## Usage

``` r
simulate_random_walk(
  initial_average_occurrences = 50,
  n_time_points = 10,
  sd_step = 0.05,
  seed = NA
)
```

## Arguments

- initial_average_occurrences:

  A positive numeric value indicating the average number of occurrences
  to be simulated at the first time point.

- n_time_points:

  A positive integer specifying the number of time points to simulate.

- sd_step:

  A positive numeric value indicating the standard deviation of the
  random steps.

- seed:

  A positive numeric value setting the seed for random number generation
  to ensure reproducibility. If `NA` (default), then
  [`set.seed()`](https://rdrr.io/r/base/Random.html) is not called at
  all. If not `NA`, then the random number generator state is reset (to
  the state before calling this function) upon exiting this function.

## Value

A vector of integers of length `n_time_points` with the average number
of occurrences.

## See also

Other occurrence:
[`create_spatial_pattern()`](https://b-cubed-eu.github.io/gcube/reference/create_spatial_pattern.md),
[`sample_occurrences_from_raster()`](https://b-cubed-eu.github.io/gcube/reference/sample_occurrences_from_raster.md),
[`simulate_timeseries()`](https://b-cubed-eu.github.io/gcube/reference/simulate_timeseries.md)

## Examples

``` r
simulate_random_walk(
  initial_average_occurrences = 50,
  n_time_points = 10,
  sd_step = 1,
  seed = 123
)
#>  [1] 50.00000 49.43952 49.20935 50.76806 50.83856 50.96785 52.68292 53.14383
#>  [9] 51.87877 51.19192
```
