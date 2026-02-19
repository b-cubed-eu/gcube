# Convert virtualspecies sample to `sf` format

Converts virtual species samples generated with the virtualspecies
package into a spatial (`sf`) object compatible with gcube workflows.
Optionally extracts values from raster layers at the sample locations.

## Usage

``` r
virtualsample_to_sf(virtual_sample, raster_lyr = NULL)
```

## Arguments

- virtual_sample:

  A list output from
  [`virtualspecies::sampleOccurrences()`](https://rdrr.io/pkg/virtualspecies/man/sampleOccurrences.html),
  containing `sample.points`, a data frame with columns `x` and `y`, and
  `original.distribution.raster`, a
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object.

- raster_lyr:

  Optional. A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  from which to extract values at sample locations. For example, habitat
  suitability or probability of occurrence rasters.

## Value

An `sf` object (point geometry) with the following columns:

- id:

  A character ID for each sample point (based on row names of
  `sample.points`).

- observed:

  Logical value indicating if the sample was observed (`TRUE`) or a
  non-detection (`FALSE`), based on the `Observed` column.

- ...:

  Any additional columns from `sample.points` or extracted from the
  raster layer(s).

- geometry:

  Point geometry in the coordinate reference system of the original
  distribution raster.

## Details

This function is typically used as the first step after sampling from a
virtual species distribution before applying functions like
[`filter_observations()`](https://b-cubed-eu.github.io/gcube/reference/filter_observations.md),
[`add_coordinate_uncertainty()`](https://b-cubed-eu.github.io/gcube/reference/add_coordinate_uncertainty.md),
and
[`grid_designation()`](https://b-cubed-eu.github.io/gcube/reference/grid_designation.md).
See the tutorial ["Create occurrence cubes for virtual
species"](https://b-cubed-eu.github.io/gcube/articles/cube-for-virtual-species.html)
for a full workflow example.

If raster layers are provided through `raster_lyr`, the values at each
point are extracted using
[`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
and appended to the output.

## See also

Other main:
[`add_coordinate_uncertainty()`](https://b-cubed-eu.github.io/gcube/reference/add_coordinate_uncertainty.md),
[`filter_observations()`](https://b-cubed-eu.github.io/gcube/reference/filter_observations.md),
[`grid_designation()`](https://b-cubed-eu.github.io/gcube/reference/grid_designation.md),
[`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md),
[`simulate_occurrences()`](https://b-cubed-eu.github.io/gcube/reference/simulate_occurrences.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# After generating a virtual species and sampling occurrences
# with virtualspecies::sampleOccurrences()
virtualsample_to_sf(virtual_sample)

# Optionally extract suitability and occurrence probability
virtualsample_to_sf(
  virtual_sample,
  raster_lyr = c(virtual_species$suitab.raster,
                 virtual_species$probability.of.occurrence)
)
} # }
```
