# Prepare example datasets
## number of points and extend
n_points <- 4
xlim <- c(3841000, 3842000)
ylim <- c(3110000, 3112000)

## set seed for reproducible observations object
set.seed(9)

## dataset without coordinateUncertaintyInMeters
observations_sf1 <- data.frame(
  lat = runif(n_points, ylim[1], ylim[2]),
  long = runif(n_points, xlim[1], xlim[2]),
  time_point = 1
  ) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 3035)

## dataset with coordinateUncertaintyInMeters
coordinate_uncertainty <- rgamma(n_points, shape = 5, rate = 0.1)
observations_sf2 <- observations_sf1 %>%
  dplyr::mutate(coordinateUncertaintyInMeters = coordinate_uncertainty)

## dataset without geometry
observations_sf3 <- observations_sf2 %>%
  sf::st_drop_geometry()

# Add buffer uncertainty in meters around points
observations_sf2_buffered <- observations_sf2 %>%
  sf::st_buffer(observations_sf2$coordinateUncertaintyInMeters)

# Create grid
grid_df1 <- sf::st_make_grid(
  observations_sf2_buffered,
  square = TRUE,
  cellsize = c(200, 200)
  ) %>%
  sf::st_sf()

grid_df2 <- grid_df1 %>%
  dplyr::mutate(id = seq_len(nrow(grid_df1)))

## grid without geometry
grid_df3 <- grid_df1 %>%
  sf::st_drop_geometry()

# Unit tests
test_that("unique ids if id column is provided", {
  expect_warning(
    grid_designation(observations_sf2,
                     grid = grid_df1 %>%
                       dplyr::mutate(id = 1),
                     id_col = "id"),
    regexp = paste0(
      "Column 'id' does not contain unique ids for grid cells!\n",
      "Creating new ids based on row names."
    ),
    fixed = TRUE)
})

test_that("provided id column present in provided grid", {
  expect_warning(
    grid_designation(observations_sf2,
                     grid = grid_df1 %>%
                       dplyr::mutate(id = seq_len(nrow(grid_df1))),
                     id_col = "identifier"),
    regexp = paste0(
      "Column name 'identifier' not present in provided grid!\n",
      "Creating ids based on row names."
    ),
    fixed = TRUE)
})

## expected outputs
test_that("output class is correct", {
  # aggregate is TRUE
  suppressWarnings({
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1),
                    class = "sf")
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1),
                    class = "data.frame")
  })
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1),
                  class = "sf")
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1),
                  class = "data.frame")

  # aggregate is FALSE
  suppressWarnings({
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1,
                                     aggregate = FALSE),
                    class = "sf")
    expect_s3_class(grid_designation(observations_sf1, grid = grid_df1,
                                     aggregate = FALSE),
                    class = "data.frame")
  })
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1,
                                   aggregate = FALSE),
                  class = "sf")
  expect_s3_class(grid_designation(observations_sf2, grid = grid_df1,
                                   aggregate = FALSE),
                  class = "data.frame")
})

test_that("correct column names present", {
  # aggregate = TRUE, randomisation = "uniform"
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1)),
                    c("id", "n", "min_coord_uncertainty", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1)),
                  c("id", "n", "min_coord_uncertainty", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        dplyr::mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier")),
    c("identifier", "n", "min_coord_uncertainty", "geometry"))

  # aggregate = TRUE, randomisation = "normal"
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1,
                                           randomisation = "normal")),
                    c("id", "n", "min_coord_uncertainty", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1,
                                         randomisation = "normal")),
                  c("id", "n", "min_coord_uncertainty", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        dplyr::mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier",
      randomisation = "normal")),
    c("identifier", "n", "min_coord_uncertainty", "geometry"))

  # aggregate = FALSE, randomisation = "uniform"
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1,
                                           aggregate = FALSE)),
                    c("id", "coordinateUncertaintyInMeters", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1,
                                         aggregate = FALSE)),
                  c("id", "coordinateUncertaintyInMeters", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        dplyr::mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier",
      aggregate = FALSE)),
    c("identifier", "coordinateUncertaintyInMeters", "geometry"))

  # aggregate = FALSE, randomisation = "normal"
  suppressWarnings({
    expect_contains(names(grid_designation(observations_sf1, grid = grid_df1,
                                           aggregate = FALSE,
                                           randomisation = "normal")),
                    c("id", "coordinateUncertaintyInMeters", "geometry"))
  })
  expect_contains(names(grid_designation(observations_sf2, grid = grid_df1,
                                         aggregate = FALSE,
                                         randomisation = "normal")),
                  c("id", "coordinateUncertaintyInMeters", "geometry"))
  expect_contains(
    names(grid_designation(
      observations_sf2,
      grid = grid_df1 %>%
        dplyr::mutate(identifier = seq_len(nrow(grid_df1))),
      id_col = "identifier",
      aggregate = FALSE,
      randomisation = "normal")),
    c("identifier", "coordinateUncertaintyInMeters", "geometry"))
})

test_that("no minimal coordinate uncertainty for empty grid cells", {
  suppressWarnings({
  grid_designation_df1 <- grid_designation(observations_sf1, grid = grid_df1)
  grid_designation_df2 <- grid_designation(observations_sf2, grid = grid_df1)
  grid_designation_df3 <- grid_designation(observations_sf1, grid = grid_df1,
                                           randomisation = "normal")
  grid_designation_df4 <- grid_designation(observations_sf2, grid = grid_df1,
                                           randomisation = "normal")
  })

  # randomisation is "uniform"
  expect_equal(sum(grid_designation_df1$n == 0),
               sum(is.na(grid_designation_df1$min_coord_uncertainty))
               )

  expect_equal(sum(grid_designation_df2$n == 0),
    sum(is.na(grid_designation_df2$min_coord_uncertainty))
  )

  # randomisation is "normal"
  expect_equal(sum(grid_designation_df3$n == 0),
    sum(is.na(grid_designation_df3$min_coord_uncertainty))
  )

  expect_equal(sum(grid_designation_df4$n == 0),
    sum(is.na(grid_designation_df4$min_coord_uncertainty))
  )
})

# Calculate all potential grid cells for the observations
sf::st_agr(observations_sf1) <- "constant"
sf::st_agr(observations_sf2_buffered) <- "constant"
sf::st_agr(grid_df2) <- "constant"
# No uncertainty
potential_gridcells_sf1 <- sf::st_intersection(grid_df2, observations_sf1) %>%
  dplyr::pull(id)
# With uncertainty
potential_gridcells_sf2 <- sf::st_intersection(grid_df2,
                                           observations_sf2_buffered) %>%
  dplyr::pull(id)

test_that("check possible outcomes for grid cell designation", {
  # aggregate = TRUE, randomisation = "uniform"
  suppressWarnings({
    expect_contains(potential_gridcells_sf1,
                    grid_designation(observations_sf1, grid = grid_df2,
                                     id_col = "id") %>%
                      dplyr::filter(n > 0) %>%
                      dplyr::pull(id))
  })
  expect_contains(potential_gridcells_sf2,
                  grid_designation(observations_sf2, grid = grid_df2,
                                   id_col = "id") %>%
                    dplyr::filter(n > 0) %>%
                    dplyr::pull(id))
  # aggregate = TRUE, randomisation = "normal"
  suppressWarnings({
    expect_contains(potential_gridcells_sf1,
                    grid_designation(observations_sf1, grid = grid_df2,
                                     id_col = "id",
                                     randomisation = "normal") %>%
                      dplyr::filter(n > 0) %>%
                      dplyr::pull(id))
  })
  expect_contains(potential_gridcells_sf2,
                  grid_designation(observations_sf2, grid = grid_df2,
                                   id_col = "id",
                                   randomisation = "normal") %>%
                    dplyr::filter(n > 0) %>%
                    dplyr::pull(id))

  # aggregate = FALSE, randomisation = "uniform"
  suppressWarnings({
    expect_contains(potential_gridcells_sf1,
                    grid_designation(observations_sf1, grid = grid_df2,
                                     id_col = "id",
                                     aggregate = FALSE) %>%
                     dplyr::pull(id))
  })
  expect_contains(potential_gridcells_sf2,
                  grid_designation(observations_sf2, grid = grid_df2,
                                   id_col = "id",
                                   aggregate = FALSE) %>%
                    dplyr::pull(id))
  # aggregate = FALSE, randomisation = "normal"
  suppressWarnings({
    expect_contains(potential_gridcells_sf1,
                    grid_designation(observations_sf1, grid = grid_df2,
                                     id_col = "id",
                                     randomisation = "normal",
                                     aggregate = FALSE) %>%
                      dplyr::pull(id))
  })
  expect_contains(potential_gridcells_sf2,
                  grid_designation(observations_sf2, grid = grid_df2,
                                   id_col = "id",
                                   randomisation = "normal",
                                   aggregate = FALSE) %>%
                    dplyr::pull(id))
})

test_that("number of observations should equal numbers in grid", {
  # randomisation is "uniform"
  suppressWarnings({
    expect_equal(grid_designation(observations_sf1, grid = grid_df1) %>%
                   dplyr::pull(n) %>%
                   sum(),
                 nrow(observations_sf1))
  })
  expect_equal(grid_designation(observations_sf2, grid = grid_df1) %>%
                 dplyr::pull(n) %>%
                 sum(),
               nrow(observations_sf2))
  # randomisation is "normal"
  suppressWarnings({
    expect_equal(grid_designation(observations_sf1, grid = grid_df1,
                                  randomisation = "normal") %>%
                   dplyr::pull(n) %>%
                   sum(),
                 nrow(observations_sf1))
  })
  expect_equal(grid_designation(observations_sf2, grid = grid_df1,
                                randomisation = "normal") %>%
                 dplyr::pull(n) %>%
                 sum(),
               nrow(observations_sf2))
})

test_that("number of observations be the same as output if aggregate = FALSE", {
  # randomisation is "uniform"
  suppressWarnings({
    expect_equal(grid_designation(observations_sf1, grid = grid_df1,
                                  aggregate = FALSE) %>%
                   nrow(),
                 nrow(observations_sf1))
  })
  expect_equal(grid_designation(observations_sf2, grid = grid_df1,
                                aggregate = FALSE) %>%
                 nrow(),
               nrow(observations_sf2))
  # randomisation is "normal"
  suppressWarnings({
    expect_equal(grid_designation(observations_sf1, grid = grid_df1,
                                  randomisation = "normal",
                                  aggregate = FALSE) %>%
                   nrow(),
                 nrow(observations_sf1))
  })
  expect_equal(grid_designation(observations_sf2, grid = grid_df1,
                                randomisation = "normal",
                                aggregate = FALSE) %>%
                 nrow(),
               nrow(observations_sf2))
})

test_that("CRS of input observations and output are the same", {
  # Custom CRS for the test
  custom_crs <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"

  # Create observations with custom CRS
  observations_sf_custom_crs <- sf::st_transform(observations_sf2,
                                                 crs = custom_crs)
  grid_sf_custom_crs <- sf::st_transform(grid_df1,
                                         crs = custom_crs)

  # Apply grid_designation function
  output_sf <- grid_designation(observations_sf_custom_crs,
                                grid = grid_sf_custom_crs)

  # Check if CRS is the same
  expect_equal(sf::st_crs(observations_sf_custom_crs), sf::st_crs(output_sf))
})

test_that("grid_designation raises an error for incorrect observations type", {
  expect_error(grid_designation(observations_sf3, grid_df1),
               "`observations` must be an sf object.")
})

test_that("grid_designation raises an error for incorrect grid type", {
  expect_error(grid_designation(observations_sf2, grid_df3),
               "`grid` must be an sf object.")
})

test_that("grid_designation raises an error for incorrect id_col type", {
  expect_error(grid_designation(observations_sf2, grid_df1, id_col = 123),
               "`id_col` must be a character vector of length 1.")
})

test_that("grid_designation raises an error for incorrect aggregate type", {
  expect_error(
    grid_designation(observations_sf2, grid_df1, aggregate = "not_logical"),
    "`aggregate` must be a logical vector of length 1.")
})

test_that("grid_designation raises an error for different CRS", {
  grid_df1_different_crs <- st_transform(grid_df1, crs = 4326)
  expect_error(grid_designation(observations_sf2, grid_df1_different_crs),
               "`grid` must have the same CRS as `observations`.")
})

test_that("grid_designation raises an error for invalid randomisation value", {
  expect_error(
    grid_designation(observations_sf2, grid_df1, randomisation = "invalid"),
    "`randomisation` must be one of 'uniform', 'normal'.")
})
