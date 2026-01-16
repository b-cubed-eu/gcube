# Generate a taxonomic hierarchy

This function generates a random taxonomic hierarchy for a specified
numbers of species, genera, families, orders, classes, phyla, and
kingdoms. The output is a data frame with the hierarchical
classification for each species.

## Usage

``` r
generate_taxonomy(
  num_species,
  num_genera,
  num_families,
  num_orders = 1,
  num_classes = 1,
  num_phyla = 1,
  num_kingdoms = 1,
  seed = NA
)
```

## Arguments

- num_species:

  Number of species to generate, or a dataframe. With a dataframe, the
  function will create a species with taxonomic hierarchy for each row.
  The original columns of the dataframe will be retained in the output.

- num_genera:

  Number of genera to generate.

- num_families:

  Number of families to generate.

- num_orders:

  Number of orders to generate. Defaults to 1.

- num_classes:

  Number of classes to generate. Defaults to 1.

- num_phyla:

  Number of phyla to generate. Defaults to 1.

- num_kingdoms:

  Number of kingdoms to generate. Defaults to 1.

- seed:

  A positive numeric value setting the seed for random number generation
  to ensure reproducibility. If `NA` (default), then
  [`set.seed()`](https://rdrr.io/r/base/Random.html) is not called at
  all. If not `NA`, then the random number generator state is reset (to
  the state before calling this function) upon exiting this function.

## Value

A data frame with the taxonomic classification of each species. If
`num_species` is a dataframe, the taxonomic classification is added to
this input dataframe. The original columns of the dataframe will be
retained in the output.

## Details

The function works by randomly assigning species to genera, genera to
families, families to orders, orders to classes, classes to phyla, and
phyla to kingdoms. Sampling is done with replacement, allowing multiple
lower-level taxa (e.g., species) to be assigned to the same higher-level
taxon (e.g., genus).

## See also

Other multispecies:
[`map_add_coordinate_uncertainty()`](https://b-cubed-eu.github.io/gcube/reference/map_add_coordinate_uncertainty.md),
[`map_filter_observations()`](https://b-cubed-eu.github.io/gcube/reference/map_filter_observations.md),
[`map_grid_designation()`](https://b-cubed-eu.github.io/gcube/reference/map_grid_designation.md),
[`map_sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/map_sample_observations.md),
[`map_simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/map_simulate_occurrences.md)

## Examples

``` r
# 1. Create simple taxonomic hierarchy
generate_taxonomy(
  num_species = 5,
  num_genera = 3,
  num_families = 2,
  seed = 123)
#>    species species_key  genus  family  order  class  phylum  kingdom
#> 1 species1           1 genus3 family2 order1 class1 phylum1 kingdom1
#> 2 species2           2 genus3 family2 order1 class1 phylum1 kingdom1
#> 3 species3           3 genus3 family2 order1 class1 phylum1 kingdom1
#> 4 species4           4 genus2 family2 order1 class1 phylum1 kingdom1
#> 5 species5           5 genus3 family2 order1 class1 phylum1 kingdom1

# 2. Add taxonomic hierarchy to a dataframe
existing_df <- data.frame(
  count = c(1, 2, 5, 4, 8, 9, 3),
  det_prob = c(0.9, 0.9, 0.9, 0.8, 0.5, 0.2, 0.2)
  )

generate_taxonomy(
  num_species = existing_df,
  num_genera = 4,
  num_families = 2,
  seed = 125)
#>    species species_key  genus  family  order  class  phylum  kingdom count
#> 1 species1           1 genus2 family1 order1 class1 phylum1 kingdom1     1
#> 2 species2           2 genus2 family1 order1 class1 phylum1 kingdom1     2
#> 3 species3           3 genus3 family2 order1 class1 phylum1 kingdom1     5
#> 4 species4           4 genus4 family2 order1 class1 phylum1 kingdom1     4
#> 5 species5           5 genus4 family2 order1 class1 phylum1 kingdom1     8
#> 6 species6           6 genus3 family2 order1 class1 phylum1 kingdom1     9
#> 7 species7           7 genus1 family1 order1 class1 phylum1 kingdom1     3
#>   det_prob
#> 1      0.9
#> 2      0.9
#> 3      0.9
#> 4      0.8
#> 5      0.5
#> 6      0.2
#> 7      0.2
```
