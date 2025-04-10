virtualsample_to_sf <- function(virtual_sample) {
  # Virtual samples to sf object
  sample_sf <- sf::st_as_sf(
      virtual_sample$sample.points,
      coords = c("x", "y"),
      crs = terra::crs(virtual_sample$original.distribution.raster)
    )

  # Add information to dataset and select output columns
  det_prob <- virtual_sample$detection.probability$detection.probability
  sample_sf %>%
    dplyr::mutate(
      id = rownames(virtual_sample$sample.points),
      detection_probability = det_prob,
      observed = !is.na(.data$Observed)) %>%
    dplyr::select("id", "observed", "geometry")
}
