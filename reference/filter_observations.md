# Filter detected occurrences

This function filters observations from all occurrences based on the
`observed` column, typically created by the
[`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md)
function.

## Usage

``` r
filter_observations(observations_total, invert = FALSE)
```

## Arguments

- observations_total:

  An sf object with POINT geometry or a simple dataframe with `observed`
  column containing logical values (`TRUE`/`FALSE`). This format is
  typically created by the
  [`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md)
  function.

- invert:

  Logical. If `FALSE` (default), the function filters to retain only
  observed occurrences. If `TRUE`, it filters out unobserved
  occurrences.

## Value

A data frame or an sf object with POINT geometry containing the filtered
observations. If `invert = FALSE`, the function returns observed
occurrences. If `invert = TRUE`, it returns unobserved occurrences.

## See also

Other main:
[`add_coordinate_uncertainty()`](https://b-cubed-eu.github.io/gcube/reference/add_coordinate_uncertainty.md),
[`grid_designation()`](https://b-cubed-eu.github.io/gcube/reference/grid_designation.md),
[`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md),
[`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md),
[`virtualsample_to_sf()`](https://b-cubed-eu.github.io/gcube/reference/virtualsample_to_sf.md)

## Examples

``` r
# Create dataframe with sampling status column
occurrences_data <- data.frame(
    time_point = 1,
    sampling_prob = seq(0.5, 1, 0.1),
    observed = rep(c(FALSE, TRUE), each = 3)
  )

# Keep detected occurrences
filter_observations(occurrences_data)
#>   time_point sampling_prob observed
#> 4          1           0.8     TRUE
#> 5          1           0.9     TRUE
#> 6          1           1.0     TRUE

# Keep undetected occurrences
filter_observations(occurrences_data, invert = TRUE)
#>   time_point sampling_prob observed
#> 1          1           0.5    FALSE
#> 2          1           0.6    FALSE
#> 3          1           0.7    FALSE
```
