# gcube 1.3.0

* Fix mistake in multispecies vignette #108
* Add `progress` argument to `map_*` functions #109
* Add default value for missing `coordinateUncertaintyInMeters` in grid
  designation #114
* Change `sampling_status` column in output `sample_observations()` to logical
  output column called `observed`
* Create `virtualsample_to_sf()` function to work with virtual species #75
* Check package with `lintr::indentation_linter()`

# gcube 1.2.0

* Add tutorial to work with virtual species.

# gcube 1.1.2

* Join the [R-universe](https://b-cubed-eu.r-universe.dev/)!

# gcube 1.1.1

* Add more elaborate package description #102.
* Uniform naming of articles and start headings at h2 #103.
* Add DOI to citation file #104.

# gcube 1.1.0

* Publish release on Zenodo.

# gcube 1.0.0

* Add package origin story.

# gcube 0.4.0

*	Consolidate documentation across all functions, README, and vignettes.
* Update `sample_occurrences_from_raster()`
  - Use `lapply()` instead of for-loop
  - Randomise points in raster cells.
* Fix issues (#37, #70, #76).

# gcube 0.3.0

*	`generate_taxonomy()` also creates species key.
*	Fix bug for mapping simulate occurrences without specified temporal function.

# gcube 0.2.0

*	Improve function checks with `stopifnot()` and the [assertthat](https://CRAN.R-project.org/package=assertthat) package.
*	Add missing unit tests.
*	Set repo status to active.
* Use tidy contributing and code of conduct.

# gcube 0.1.0

* Create data cube for multiple species using `purrr::pmap()`.
* Generate species taxonomy.
* Change vignettes into articles.
* Create article for multi-species approach.
* Add name rationale to README.
* Clarify pkgdown website page names.

# gcube 0.0.1

* Add [checklist](https://inbo.github.io/checklist/) infrastructure.

# gcube 0.0.0

* Added a `NEWS.md` file to track changes to the package.
