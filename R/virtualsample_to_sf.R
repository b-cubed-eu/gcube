virtualsample_to_sf <- function(virtual_sample, raster_lyr = NULL) {
  # Virtual samples to sf object
  sample_sf <- sf::st_as_sf(
      virtual_sample$sample.points,
      coords = c("x", "y"),
      crs = terra::crs(virtual_sample$original.distribution.raster)
    )

  # Extract raster values if provided
  if (!is.null(raster_lyr)) {
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
