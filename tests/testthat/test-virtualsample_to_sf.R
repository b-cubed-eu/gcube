## Prepare example datasets
set.seed(123)

# Download bioclimatic data
worldclim <- geodata::worldclim_global(var = "bio", res = 10, path = tempdir())

# Western Palearctic
wp <- terra::ext(-15, 65, 30, 75)

# Crop climate data
worldclim <- terra::crop(worldclim, wp)

# Specify variables
bio_vars <- c("bio2", "bio5", "bio6", "bio13", "bio14", "bio15")

# Select variables
bio_string <- paste0("bio_", sub("bio", "", bio_vars), collapse = "|")
bio_vars_selected <- names(worldclim)[grepl(bio_string, names(worldclim))]

# Subset climate data
worldclim_vars <- worldclim[[bio_vars_selected]]

# Generate random virtual species
suppressMessages({
  virtual_species <- virtualspecies::generateRandomSp(
    worldclim_vars, plot = FALSE
  )
})

# Limit distribution
suppressWarnings({
  virtual_distribution <- virtualspecies::limitDistribution(
    x = virtual_species,
    geographical.limit = "country",
    area = c("United Kingdom", "Ireland"),
    plot = FALSE
  )
})

# Sample from virtual species distribution
virtual_sample <- virtualspecies::sampleOccurrences(
  virtual_distribution,
  n = 150,
  detection.probability = 0.8,
  plot = FALSE
)

# Prepare rasters
names(virtual_species$suitab.raster) <- "suitability"
names(virtual_species$probability.of.occurrence) <- "occ_prob"

raster_stack <- c(virtual_species$suitab.raster,
                  virtual_species$probability.of.occurrence)

## Unit tests
test_that("virtualsample_to_sf() returns expected sf object", {
  sf_obj <- virtualsample_to_sf(virtual_sample)

  expect_s3_class(sf_obj, "sf")
  expect_true(all(c("id", "observed", "geometry") %in% colnames(sf_obj)))
  expect_true(nrow(sf_obj) == nrow(virtual_sample$sample.points))
  expect_type(sf_obj$observed, "logical")
  expect_s3_class(sf::st_geometry(sf_obj), "sfc_POINT")
  expect_identical(
    sf::st_crs(sf_obj)$wkt,
    sf::st_crs(virtual_sample$original.distribution.raster)$wkt
  )
})

test_that("virtualsample_to_sf() extracts raster values correctly", {
  sf_obj <- virtualsample_to_sf(virtual_sample, raster_lyr = raster_stack)

  expect_s3_class(sf_obj, "sf")
  expect_true(
    all(
      c("id", "observed", "suitability", "occ_prob", "geometry")
      %in% colnames(sf_obj)
    )
  )
  expect_true(nrow(sf_obj) == nrow(virtual_sample$sample.points))
  expect_type(sf_obj$observed, "logical")
  expect_s3_class(sf::st_geometry(sf_obj), "sfc_POINT")
  expect_identical(
    sf::st_crs(sf_obj)$wkt,
    sf::st_crs(virtual_sample$original.distribution.raster)$wkt
  )
})

test_that("virtualsample_to_sf() handles incorrect input gracefully", {
  bad_input <- list(not_real = 1)
  error_message <- paste(
    "`virtual_sample` must a list output from",
    "`virtualspecies::sampleOccurrences()`,\ncontaining `sample.points` and",
    "`original.distribution.raster`."
  )
  expect_error(
    virtualsample_to_sf(bad_input),
    regexp = error_message,
    fixed = TRUE
  )

  fake_sample <- list(
    sample.points = data.frame(x = 1:2, y = 1:2),
    original.distribution.raster = "not_a_raster"
  )
  class(fake_sample) <- c("VSSampledPoints", "list")
  expect_error(
    virtualsample_to_sf(fake_sample),
    regexp = error_message,
    fixed = TRUE
  )

  expect_error(
    virtualsample_to_sf(virtual_sample, raster_lyr = "not_a_raster"),
    regexp = "`raster_lyr` must a `terra::SpatRaster` object.",
    fixed = TRUE
  )
})
