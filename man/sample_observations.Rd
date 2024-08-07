% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_observations.R
\name{sample_observations}
\alias{sample_observations}
\title{Sample observations from a larger occurrence dataset}
\usage{
sample_observations(
  occurrences,
  detection_probability = 1,
  sampling_bias = c("no_bias", "polygon", "manual"),
  bias_area = NA,
  bias_strength = 1,
  bias_weights = NA,
  seed = NA
)
}
\arguments{
\item{occurrences}{An sf object with POINT geometry.}

\item{detection_probability}{A numeric value between 0 and 1, corresponding
to the probability of detection of the species.}

\item{sampling_bias}{\code{"no_bias"}, \code{"polygon"} or \code{"manual"}. The method used
to generate a sampling bias. \code{"polygon"}: bias the sampling in a polygon.
Provide your polygon to \code{bias_area}. Provide bias strength to
\code{bias_strength}. \code{"manual"}: bias the sampling manually via a raster.
Provide your raster layer in which each cell contains the probability to be
sampled to \code{bias_weights}.}

\item{bias_area}{\code{NA} or an sf object with POLYGON geometry. Only used if
\code{sampling_bias = "polygon"}. The area in which the sampling will be biased.}

\item{bias_strength}{\code{NA} or a positive numeric value. Only used if
\code{sampling_bias = "polygon"}. The strength of the bias to be applied in the
biased area (as a multiplier). Above 1, area will be oversampled. Below 1,
area will be undersampled. For example, a value of 50 will result in a 50
times sampling probability within the \code{bias_area} than outside. Conversely,
a value of 0.5 will result in half less samples within the \code{bias_area} than
outside.}

\item{bias_weights}{\code{NA} or a raster layer (sf object with POLYGON geometry).
Only used if \code{sampling_bias = "manual"}. The raster of bias weights to be
applied to the sampling of occurrences. Higher weights mean a higher
probability of sampling. Weights can be numeric values between 0 and 1 or
positive integers that will be rescaled to values between 0 and 1.}

\item{seed}{A positive numeric value. The seed for random number generation
to make results reproducible. If \code{NA} (the default), no seed is used.}
}
\value{
An sf object with POINT geometry containing the locations of the
sampled observations, a \code{detection_probability} column containing the
detection probability for each observation (will be the same for all), a
\code{bias_weight} column containing the sampling probability based on sampling
bias, a \code{sampling_probability} column containing the combined sampling
probability from detection probability and sampling bias for each
observation, and a \code{sampling_status} column indicating whether the
occurrence was detected (observations) or not (unobserved occurrences).
}
\description{
The function samples observations from occurrences based on detection
probability and sampling bias by implementing a Bernoulli trial.
}
\examples{
# Load packages
library(sf)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Simulate some occurrence data with coordinates and time points
num_points <- 10
occurrences <- data.frame(
  lon = runif(num_points, min = -180, max = 180),
  lat = runif(num_points, min = -90, max = 90),
  time_point = 0
  )

# Convert the occurrence data to an sf object
occurrences_sf <- st_as_sf(occurrences, coords = c("lon", "lat"))

# Sample observations without sampling bias
sample_observations(
  occurrences_sf,
  detection_probability = 0.8,
  sampling_bias = "no_bias",
  seed = 123
  )

# Sample observations with sampling bias in a polygon
# Create bias_area polygon overlapping two of the points
selected_observations <- st_union(occurrences_sf[2:3,])
bias_area <- st_convex_hull(selected_observations) \%>\%
  st_buffer(dist = 100) \%>\%
  st_as_sf()

sample_observations(
  occurrences_sf,
  detection_probability = 0.8,
  sampling_bias = "polygon",
  bias_area = bias_area,
  bias_strength = 2,
  seed = 123
  )

# Sample observations with sampling bias given manually in a grid
# Create raster grid with bias weights between 0 and 1
grid <- st_make_grid(occurrences_sf) \%>\%
  st_sf() \%>\%
  mutate(bias_weight = runif(n(), min = 0, max = 1))

sample_observations(
  occurrences_sf,
  detection_probability = 0.8,
  sampling_bias = "manual",
  bias_weights = grid,
  seed = 123
  )
}
\seealso{
Other main: 
\code{\link{add_coordinate_uncertainty}()},
\code{\link{filter_observations}()},
\code{\link{grid_designation}()},
\code{\link{simulate_occurrences}()}
}
\concept{main}
