When working with WGS84 (or any geographic coordinate system using degrees), you can't directly use a Euclidean circle because degrees are not uniform in distance (they vary with latitude). Instead, you should:

1. Generate a random displacement in meters.
2. Convert this displacement from meters to degrees.
3. Apply the displacement to the latitude and longitude while accounting for Earth's curvature.

Here's how you can modify your approach:

### **Key Adjustments**
- Convert uncertainty in meters to degrees:
  - 1° of latitude ≈ 111,320 m
  - 1° of longitude depends on latitude: \( \cos(\text{lat}) \times 111,320 \) m
- Apply a random displacement in a circle.

---

### **Updated Code for WGS84**
```r
# Check if coordinates are in degrees
is_degree <- sf::st_crs(observations)$units_gdal == "degree"

# Function to convert meters to degrees
meters_to_degrees <- function(meters, latitude) {
  lat_degree = meters / 111320
  lon_degree = meters / (111320 * cos(latitude * pi / 180))
  return(list(lat = lat_degree, lon = lon_degree))
}

# Generate random offsets
uncertainty_points <-
  observations %>%
  dplyr::mutate(
    random_angle = stats::runif(nrow(observations), 0, 2 * pi),
    random_r = sqrt(stats::runif(nrow(observations), 0, 1)) *
      .data$coordinateUncertaintyInMeters
  )

# Calculate new points considering degrees
new_points <-
  uncertainty_points %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    lat = sf::st_coordinates(.data$geometry)[2],
    lon = sf::st_coordinates(.data$geometry)[1],
    displacement = meters_to_degrees(random_r, lat),
    x_new = lon + displacement$lon * cos(random_angle),
    y_new = lat + displacement$lat * sin(random_angle)
  ) %>%
  sf::st_drop_geometry() %>%
  sf::st_as_sf(
    coords = c("x_new", "y_new"),
    crs = sf::st_crs(observations)
  )
```

### **Explanation**
1. Convert the random distance (`random_r`) from meters to degrees.
2. Adjust longitude conversion based on latitude.
3. Apply random movement in a circular pattern using trigonometry.
4. Generate new points and create an updated `sf` object.

---

This method ensures that the random points are correctly placed within a circle, even when working in degrees (e.g., WGS84). Let me know if you need any refinements! 🚀
