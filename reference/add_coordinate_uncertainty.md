# Add coordinate uncertainty to observations

This function adds a column to the input dataframe or sf object
containing the coordinate uncertainty for each observation, measured in
meters.

## Usage

``` r
add_coordinate_uncertainty(observations, coords_uncertainty_meters = 25)
```

## Arguments

- observations:

  An sf object with POINT geometry or a simple dataframe representing
  the observations. This object contains the observation points to which
  the coordinate uncertainty will be added.

- coords_uncertainty_meters:

  A numeric value or a vector of numeric values representing the
  coordinate uncertainty (in meters) associated with each observation.
  If a single numeric value is provided, it will be applied to all
  observations. If a numeric vector is provided, it must be the same
  length as the number of observations.

## Value

The input data frame or an sf object with POINT geometry, with an
additional column named `coordinateUncertaintyInMeters` that contains
the coordinate uncertainty values in meters.

## See also

Other main:
[`filter_observations()`](https://b-cubed-eu.github.io/gcube/reference/filter_observations.md),
[`grid_designation()`](https://b-cubed-eu.github.io/gcube/reference/grid_designation.md),
[`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md),
[`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md),
[`virtualsample_to_sf()`](https://b-cubed-eu.github.io/gcube/reference/virtualsample_to_sf.md)

## Examples

``` r
# Create dataframe with sampling status column
observations_data <- data.frame(
    time_point = 1,
    sampling_prob = seq(0.5, 1, 0.1)
  )

# provide a fixed uncertainty for all points
add_coordinate_uncertainty(
  observations_data,
  coords_uncertainty_meters = 1000
 )
#>   time_point sampling_prob coordinateUncertaintyInMeters
#> 1          1           0.5                          1000
#> 2          1           0.6                          1000
#> 3          1           0.7                          1000
#> 4          1           0.8                          1000
#> 5          1           0.9                          1000
#> 6          1           1.0                          1000

# add variability in uncertainty. For example, using gamma distribution
uncertainty_vec <- seq(50, 100, 10)

add_coordinate_uncertainty(
  observations_data,
  coords_uncertainty_meters = uncertainty_vec
)
#>   time_point sampling_prob coordinateUncertaintyInMeters
#> 1          1           0.5                            50
#> 2          1           0.6                            60
#> 3          1           0.7                            70
#> 4          1           0.8                            80
#> 5          1           0.9                            90
#> 6          1           1.0                           100
```
