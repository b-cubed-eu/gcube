# 1. Simulating the occurrence process

The workflow for simulating a biodiversity data cube used in gcube can
be divided in three steps or processes:

1.  Occurrence process
2.  Detection process
3.  Grid designation process

This tutorial documents the first part of the gcube simulation workflow,
viz. the occurrence process.

``` r

# Load packages
library(gcube)

library(sf)        # work with spatial objects
library(dplyr)     # data wrangling
library(ggplot2)   # data visualisation
library(tidyterra) # visualisation spatraster objects
```

## Input

The functions are set up such that a single polygon as input is enough
to go through this workflow using default arguments. The user can change
these arguments to allow for more flexibility. In this tutorial we will
demonstrate the different options.

As input, we create a polygon in which we want to simulate occurrences.
It represents the spatial extent of the species.

``` r

polygon <- st_polygon(list(cbind(c(500, 1000, 1000, 600, 200, 100, 500),
                                 c(200, 100, 700, 1000, 900, 500, 200))))
```

The polygon looks like this.

``` r

ggplot() +
  geom_sf(data = polygon) +
  theme_minimal()
```

![Spatial extent in which we will simulate species
occurrences.](occurrence-process_files/figure-html/unnamed-chunk-3-1.png)

## Simulate occurrences

We generate occurrence points within the polygon using the
[`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md)
function. Default arguments ensure that an sf object with POLYGON
geometry is sufficient to simulate occurrences.

``` r

?simulate_occurrences
```

The options for user defined arguments are demonstrated in the next
subsections.

### Changing number of occurrences over time

Say we want to have 100 occurrences in our plot over 10 years. You can
change the trend in the average number of occurrences over time. We
visualise this with the supporting functions used in
[`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md).
The number of occurrences are always drawn from a Poisson distribution.

``` r

?simulate_timeseries
```

**Option 1**

If we do not specify a temporal function, we draw from a Poisson
distribution for each time point with average (lambda parameter)
`initial_average_occurrences`.

``` r

n_occurrences_indep <- simulate_timeseries(
  initial_average_occurrences = 100,
  n_time_points = 10,
  temporal_function = NA,
  seed = 123
)
```

We plot the simulated number of occurrences over time. We see that the
average is close to 100 over time as expected. Using a different seed
will result in different numbers but the average will be (close to) 100
over time.

``` r

tibble(
  n_occurrences = n_occurrences_indep,
  time_point = seq_along(n_occurrences_indep)
) %>%
  ggplot(aes(x = time_point, y = n_occurrences)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) +
  theme_minimal()
```

![Simulated number of occurrences over time with unspecified temporal
function.](occurrence-process_files/figure-html/unnamed-chunk-7-1.png)

**Option 2**

We can specify a function ourselves, e.g. the internal function
[`simulate_random_walk()`](https://b-cubed-eu.github.io/gcube/reference/simulate_random_walk.md)
to have a random walk over time. A random walk is a mathematical concept
where each step is determined randomly. The `sd_step` parameter refers
to the standard deviation of these random steps (drawn from a Normal
distribution). A higher value leading to larger steps and potentially
greater variability in the path of the random walk.

``` r

n_occurrences_walk <- simulate_timeseries(
  initial_average_occurrences = 100,
  n_time_points = 10,
  temporal_function = simulate_random_walk,
  sd_step = 1,
  seed = 123
)
```

We plot the simulated number of occurrences over time which follow a
random walk. Using a different seed will result in a different random
pattern.

``` r

tibble(
  n_occurrences = n_occurrences_walk,
  time_point = seq_along(n_occurrences_walk)
) %>%
  ggplot(aes(x = time_point, y = n_occurrences)) +
  geom_point() +
  theme_minimal()
```

![Simulated number of occurrences over time using a random walk over
time.](occurrence-process_files/figure-html/unnamed-chunk-9-1.png)

**Option 3**

We can specify a function ourselves that determines the average trend in
number of occurrences over time. Here we provide an example for a linear
trend.

``` r

my_own_linear_function <- function(
    initial_average_occurrences = initial_average_occurrences,
    n_time_points = n_time_points,
    coef) {
  # Calculate new average abundances over time
  time <- seq_len(n_time_points) - 1
  lambdas <- initial_average_occurrences + (coef * time)

  # Identify where the lambda values become 0 or lower
  zero_or_lower_index <- which(lambdas <= 0)

  # If any lambda becomes 0 or lower, set all subsequent lambdas to 0
  if (length(zero_or_lower_index) > 0) {
    zero_or_lower_indices <- zero_or_lower_index[1]:n_time_points
    lambdas[zero_or_lower_indices] <- 0
  }

  # Return average abundances
  return(lambdas)
}
```

We try out a linear trend with slope equal to 1.

``` r

n_occurrences_linear <- simulate_timeseries(
  initial_average_occurrences = 100,
  n_time_points = 10,
  temporal_function = my_own_linear_function,
  coef = 1,
  seed = 123
)
```

We plot the simulated number of occurrences over time. We see that the
average slope is indeed close to 1. Using a different seed will result
in different numbers but the average slope will be (close to) 1.

``` r

tibble(
  n_occurrences = n_occurrences_linear,
  time_point = seq_along(n_occurrences_linear)
) %>%
  ggplot(aes(x = time_point, y = n_occurrences)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) +
  theme_minimal()
```

![Simulated number of occurrences over time using a custom linear
function.](occurrence-process_files/figure-html/unnamed-chunk-12-1.png)

### Changing the degree of spatial clustering

We can also choose the amount of spatial clustering. We visualise this
with the supporting functions used in
[`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md).

``` r

?create_spatial_pattern
```

**Option 1**

There are defaults for random and clustered patterns. Let’s look at the
default where we have no clustering.

``` r

rs_pattern_random <- create_spatial_pattern(
  polygon = polygon,
  resolution = 10,
  spatial_pattern = "random",
  seed = 123
)
#> [using unconditional Gaussian simulation]
```

We see values of high sampling probability randomly distributed.

``` r

ggplot() +
  geom_spatraster(data = rs_pattern_random) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()
```

![Random spatial pattern to sample
occurrences.](occurrence-process_files/figure-html/unnamed-chunk-15-1.png)

**Option 2**

Let’s look at the default where we have clustering (same as
`spatial_pattern = 10`, see further).

``` r

rs_pattern_clustered <- create_spatial_pattern(
  polygon = polygon,
  resolution = 10,
  spatial_pattern = "clustered",
  seed = 123
)
#> [using unconditional Gaussian simulation]
```

We see values of high sampling probability clustered together.

``` r

ggplot() +
  geom_spatraster(data = rs_pattern_clustered) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()
```

![Clustered spatial pattern to sample
occurrences.](occurrence-process_files/figure-html/unnamed-chunk-17-1.png)

**Option 3**

We can also change the clustering ourselves. A larger number for
`spatial_pattern` means a broader size of the clusters area. Let’s look
at a low value for clustering.

``` r

rs_pattern_clustered2 <- create_spatial_pattern(
  polygon = polygon,
  resolution = 10,
  spatial_pattern = 5,
  seed = 123
)
#> [using unconditional Gaussian simulation]
```

We see values of high sampling probability in multiple, smaller
clusters.

``` r

ggplot() +
  geom_spatraster(data = rs_pattern_clustered2) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()
```

![Clustered spatial pattern to sample
occurrences.](occurrence-process_files/figure-html/unnamed-chunk-19-1.png)

Let’s look at a high value for clustering.

``` r

rs_pattern_clustered3 <- create_spatial_pattern(
  polygon = polygon,
  resolution = 10,
  spatial_pattern = 100,
  seed = 123
)
#> [using unconditional Gaussian simulation]
```

We see values of high sampling probability in fewer, larger clusters.

``` r

ggplot() +
  geom_spatraster(data = rs_pattern_clustered3) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()
```

![Highly clustered spatial pattern to sample
occurrences.](occurrence-process_files/figure-html/unnamed-chunk-21-1.png)

The patterns generated above are then used for sampling using a
different supporting function.

``` r

?sample_occurrences_from_raster
```

If we for example sample 500 occurrences from the last raster, we see
the sampling is according to the expected pattern.

``` r

pts_occ_clustered3 <- sample_occurrences_from_raster(
  raster = rs_pattern_clustered3,
  time_series = 500,
  seed = 123
)

ggplot() +
  geom_spatraster(data = rs_pattern_clustered3) +
  geom_sf(data = pts_occ_clustered3) +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()
```

![500 sampled occurrences from highly clustered spatial
pattern.](occurrence-process_files/figure-html/named-1.png)

## Example

Now that we know how the supporting functions work, we can generate
occurrence points within the polygon using the
[`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md)
function. We can for example sample randomly within the polygon over 6
time points were we use a random walk over time with an initial average
number of occurrences equal to 100.

``` r

occurrences_df <- simulate_occurrences(
  species_range = polygon,
  initial_average_occurrences = 100,
  n_time_points = 6,
  temporal_function = simulate_random_walk,
  sd_step = 1,
  spatial_pattern = "random",
  seed = 123
)
#> [using unconditional Gaussian simulation]
```

This is the number of occurrences we have for each time point.

``` r

occurrences_df %>%
  st_drop_geometry() %>%
  count(time_point) %>%
  ggplot(aes(x =  time_point, y = n)) +
  geom_point() +
  theme_minimal()
```

![Number of occurrences over
time.](occurrence-process_files/figure-html/unnamed-chunk-24-1.png)

This is the spatial distribution of the occurrences for each time point.

``` r

ggplot() +
  geom_sf(data = polygon) +
  geom_sf(data = occurrences_df) +
  facet_wrap(~time_point, nrow = 2) +
  ggtitle("Distribution of occurrences for each time point") +
  theme_minimal()
```

![Spatial distribution of occurrences within the polygon for each time
point.](occurrence-process_files/figure-html/unnamed-chunk-25-1.png)
