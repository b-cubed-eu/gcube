virtualsample_to_sf <- function(virtual_sample) {
  detections_df_raw <- st_as_sf(
    virtual_sample$sample.points,
    coords = c("x", "y"),
    crs = terra::crs(virtual_sample$original.distribution.raster))
}
