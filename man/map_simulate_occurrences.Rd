% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_simulate_occurrences.R
\name{map_simulate_occurrences}
\alias{map_simulate_occurrences}
\title{Map \code{simulate_occurrences()} function over multiple species}
\usage{
map_simulate_occurrences(df, nested = TRUE, arg_list = NA)
}
\arguments{
\item{df}{A dataframe containing multiple rows. Each row is considered a
different species. The columns are function arguments with values used for
mapping \code{simulate_occurrences()} for each species. \code{df} can have columns that
are not used by this function. They will be retained in the output.}

\item{nested}{Logical. If \code{TRUE} (default), retain list-column containing
sf objects calculated by \code{simulate_occurrences()}. Otherwise, expand this
list-column into rows and columns.}

\item{arg_list}{A named list or \code{NA}. If \code{NA} (default), the function assumes
column names in \code{df} are identical to argument names of
\code{simulate_occurrences()} and the function specified in its
\code{temporal_function} argument. If column names are not identical, they need to
be specified as a named list where the names are the argument names of
\code{simulate_occurrences()} or the function specified in its \code{temporal_function}
argument, and their associated values a string of the corresponding column
name in \code{df}.}
}
\value{
In case of \code{nested = TRUE}, a dataframe identical to the input
dataframe \code{df}, but with an extra list-column called \code{occurrences} containing
an sf object with POINT geometry for each row computed by
\code{simulate_occurrences()}. In case of \code{nested = FALSE}, this list-column is
expanded into additional rows and columns.
}
\description{
The function executes \code{simulate_occurrences()} over multiple rows of a
dataframe, representing multiple different species, containing potentially
different function arguments over multiple columns.
}
\examples{
# Load packages
library(sf)
library(dplyr)

# Create polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

## Example with simple column names
# Specify dataframe for 3 species with custom function arguments
species_dataset_df <- tibble(
  taxonID = c("species1", "species2", "species3"),
  plgn = rep(list(plgn), 3),
  initial_average_abundance = c(50, 100, 500),
  n_time_points = rep(6, 3),
  temporal_function = c(simulate_random_walk, simulate_random_walk, NA),
  sd_step = c(1, 1, NA),
  spatial_autocorr = "random",
  seed = 123)

# Simulate occurrences
sim_occ_nested <- map_simulate_occurrences(df = species_dataset_df)
sim_occ_nested

# Unnest output and create sf object again
sim_occ_unnested <- map_simulate_occurrences(df = species_dataset_df,
                                             nested = FALSE)
sim_occ_unnested \%>\%
   st_sf()


## Example with deviating column names
# Specify dataframe for 3 species with custom function arguments
species_dataset_df2 <- species_dataset_df \%>\%
  rename(polygon = plgn,
         sd = sd_step)

# Create named list for argument conversion
arg_conv_list <- list(
    plgn = "polygon",
    sd_step = "sd"
  )

# Simulate occurrences
map_simulate_occurrences(
  df = species_dataset_df2,
  arg_list = arg_conv_list)

map_simulate_occurrences(
  df = species_dataset_df2,
  nested = FALSE,
  arg_list = arg_conv_list)
}
\seealso{
Other multispecies: 
\code{\link{generate_taxonomy}()},
\code{\link{map_add_coordinate_uncertainty}()},
\code{\link{map_filter_observations}()},
\code{\link{map_grid_designation}()},
\code{\link{map_sample_observations}()}
}
\concept{multispecies}
