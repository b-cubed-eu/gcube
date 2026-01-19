# Changelog

## gcube 1.4.1

- Remove warning message README code

## gcube 1.4.0

- Fix installation instructions
  [\#134](https://github.com/b-cubed-eu/gcube/issues/134)
- Fix spelling mistakes
  [\#135](https://github.com/b-cubed-eu/gcube/issues/135)
- Improve funder and rights holder descriptions
  [\#136](https://github.com/b-cubed-eu/gcube/issues/136)
- Update checklist checks v0.5.2

## gcube 1.3.7

- Shorten example run time for mapping functions
  [\#130](https://github.com/b-cubed-eu/gcube/issues/130)
- Remove white line in tutorial “Grid designation for custom datasets”
  [\#131](https://github.com/b-cubed-eu/gcube/issues/131)
- Fix mistake in tutorial “Grid designation for custom datasets”
  [\#132](https://github.com/b-cubed-eu/gcube/issues/132)

## gcube 1.3.6

- Use DOI for funding

## gcube 1.3.5

- Use ROR for copyright holder

## gcube 1.3.4

- Fix NEWS file

## gcube 1.3.3

- Update tutorial grid designation own data

## gcube 1.3.2

- Add **frictionless** package to suggests

## gcube 1.3.1

- Allow grid designation for CRS in degrees
  [\#113](https://github.com/b-cubed-eu/gcube/issues/113)
- Create tutorial on grid designation of own data
  [\#115](https://github.com/b-cubed-eu/gcube/issues/115)

## gcube 1.3.0

- Fix mistake in multispecies vignette
  [\#108](https://github.com/b-cubed-eu/gcube/issues/108)
- Add `progress` argument to `map_*` functions
  [\#109](https://github.com/b-cubed-eu/gcube/issues/109)
- Add default value for missing `coordinateUncertaintyInMeters` in grid
  designation [\#114](https://github.com/b-cubed-eu/gcube/issues/114)
- Change `sampling_status` column in output
  [`sample_observations()`](https://b-cubed-eu.github.io/gcube/reference/sample_observations.md)
  to logical output column called `observed`
- Create
  [`virtualsample_to_sf()`](https://b-cubed-eu.github.io/gcube/reference/virtualsample_to_sf.md)
  function to work with virtual species
  [\#75](https://github.com/b-cubed-eu/gcube/issues/75)
- Check package with
  [`lintr::indentation_linter()`](https://lintr.r-lib.org/reference/indentation_linter.html)

## gcube 1.2.0

- Add tutorial to work with virtual species.

## gcube 1.1.2

- Join the [R-universe](https://b-cubed-eu.r-universe.dev/)!

## gcube 1.1.1

- Add more elaborate package description
  [\#102](https://github.com/b-cubed-eu/gcube/issues/102).
- Uniform naming of articles and start headings at h2
  [\#103](https://github.com/b-cubed-eu/gcube/issues/103).
- Add DOI to citation file
  [\#104](https://github.com/b-cubed-eu/gcube/issues/104).

## gcube 1.1.0

- Publish release on Zenodo.

## gcube 1.0.0

- Add package origin story.

## gcube 0.4.0

- Consolidate documentation across all functions, README, and vignettes.
- Update
  [`sample_occurrences_from_raster()`](https://b-cubed-eu.github.io/gcube/reference/sample_occurrences_from_raster.md)
  - Use [`lapply()`](https://rdrr.io/r/base/lapply.html) instead of
    for-loop
  - Randomise points in raster cells.
- Fix issues ([\#37](https://github.com/b-cubed-eu/gcube/issues/37),
  [\#70](https://github.com/b-cubed-eu/gcube/issues/70),
  [\#76](https://github.com/b-cubed-eu/gcube/issues/76)).

## gcube 0.3.0

- [`generate_taxonomy()`](https://b-cubed-eu.github.io/gcube/reference/generate_taxonomy.md)
  also creates species key.
- Fix bug for mapping simulate occurrences without specified temporal
  function.

## gcube 0.2.0

- Improve function checks with
  [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) and the
  [assertthat](https://CRAN.R-project.org/package=assertthat) package.
- Add missing unit tests.
- Set repo status to active.
- Use tidy contributing and code of conduct.

## gcube 0.1.0

- Create data cube for multiple species using
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html).
- Generate species taxonomy.
- Change vignettes into articles.
- Create article for multi-species approach.
- Add name rationale to README.
- Clarify pkgdown website page names.

## gcube 0.0.1

- Add [checklist](https://inbo.github.io/checklist/) infrastructure.

## gcube 0.0.0

- Added a `NEWS.md` file to track changes to the package.
