# nolint start: line_length_linter.
#' Convert virtualspecies sample to `sf` format
#'
#' Converts virtual species samples generated with the \pkg{virtualspecies}
#' package into a spatial (`sf`) object compatible with \pkg{gcube} workflows.
#' Optionally extracts values from raster layers at the sample locations.
#'
#' @param virtual_sample A list output from
#' `virtualspecies::sampleOccurrences()`, containing `sample.points`,
#' a data frame with columns `x` and `y`, and `original.distribution.raster`,
#' a `terra::SpatRaster` object.
#' @param raster_lyr Optional. A `terra::SpatRaster` from which to extract
#' values at sample locations. For example, habitat suitability or probability
#' of occurrence rasters.
#'
#' @return An `sf` object (point geometry) with the following columns:
#' \describe{
#'   \item{id}{A character ID for each sample point (based on row names of
#'             `sample.points`).}
#'   \item{observed}{Logical value indicating if the sample was observed
#'                   (`TRUE`) or a non-detection (`FALSE`), based on the
#'                   `Observed` column.}
#'   \item{...}{Any additional columns from `sample.points` or extracted from
#'              the raster layer(s).}
#'   \item{geometry}{Point geometry in the coordinate reference system of the
#'                   original distribution raster.}
#' }
#'
#' @details
#' This function is typically used as the first step after sampling from a
#' virtual species distribution before applying functions like
#' [gcube::filter_observations()], [gcube::add_coordinate_uncertainty()],
#' and [gcube::grid_designation()]. See the tutorial \href{https://b-cubed-eu.github.io/gcube/articles/cube-for-virtual-species.html}{
#' "Create occurrence cubes for virtual species"} for a full workflow example.
#'
#' If raster layers are provided through `raster_lyr`, the values at each point
#' are extracted using [terra::extract()] and appended to the output.
#'
#' @export
#'
#' @import dplyr
#' @import sf
#' @importFrom terra crs extract
#' @importFrom rlang .data
#'
#' @family main
#'
#' @examples
#' \dontrun{
#' # After generating a virtual species and sampling occurrences
#' # with virtualspecies::sampleOccurrences()
#' virtualsample_to_sf(virtual_sample)
#'
#' # Optionally extract suitability and occurrence probability
#' virtualsample_to_sf(
#'   virtual_sample,
#'   raster_lyr = c(virtual_species$suitab.raster,
#'                  virtual_species$probability.of.occurrence)
#' )
#' }
# nolint end

virtualsample_to_sf <- function(virtual_sample, raster_lyr = NULL) {
  ### Start checks
  error_message <- paste(
    "`virtual_sample` must a list output from",
    "`virtualspecies::sampleOccurrences()`,\ncontaining `sample.points` and",
    "`original.distribution.raster`."
  )

  do.call(stopifnot, stats::setNames(
    list(
      # Check virtual_sample object
      inherits(virtual_sample, "VSSampledPoints"),
      inherits(virtual_sample, "list"),
      # Check virtual_sample content
      "sample.points" %in% names(virtual_sample),
      inherits(virtual_sample$sample.points, "data.frame"),
      "original.distribution.raster" %in% names(virtual_sample),
      inherits(virtual_sample$original.distribution.raster, "SpatRaster")
    ),
    rep(error_message, 6)
  ))
  ### End checks

  # Virtual samples to sf object
  sample_sf <- sf::st_as_sf(
    virtual_sample$sample.points,
    coords = c("x", "y"),
    crs = terra::crs(virtual_sample$original.distribution.raster)
  )

  # Extract raster values if provided
  if (!is.null(raster_lyr)) {
    stopifnot("`raster_lyr` must a `terra::SpatRaster` object." =
                inherits(raster_lyr, "SpatRaster"))

    extracted_values <- terra::extract(raster_lyr, sample_sf, ID = FALSE)
    sample_sf <- cbind(sample_sf, extracted_values)
  }

  # Add information to dataset and select output columns
  out_df <- sample_sf %>%
    dplyr::mutate(
      id = rownames(virtual_sample$sample.points),
      observed = !is.na(.data$Observed)
    ) %>%
    dplyr::select(-c("Real", "Observed")) %>%
    dplyr::select("id", "observed", dplyr::everything(), "geometry")

  rownames(out_df) <- NULL

  return(out_df)
}
