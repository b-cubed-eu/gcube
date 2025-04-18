% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_manual_sampling_bias.R
\name{apply_manual_sampling_bias}
\alias{apply_manual_sampling_bias}
\title{Apply manual sampling bias to occurrences via a grid}
\usage{
apply_manual_sampling_bias(occurrences_sf, bias_weights)
}
\arguments{
\item{occurrences_sf}{An sf object with POINT geometry representing the
occurrences.}

\item{bias_weights}{An \code{sf} object with POLYGON geometry representing the
grid with bias weights. This sf object should contain a \code{bias_weight} column
and a \code{geometry} column. Higher weights indicate a higher probability of
sampling. Weights must be numeric values between 0 and 1 or positive
integers, which will be rescaled to values between 0 and 1.}
}
\value{
An sf object with POINT geometry that includes a \code{bias_weight}
column containing the sampling probabilities based on the sampling bias.
}
\description{
This function adds a sampling bias weight column to an sf object containing
occurrences. The sampling probabilities are based on bias weights within each
cell of a provided grid layer.
}
\examples{
# Load packages
library(sf)
library(dplyr)
library(ggplot2)

# Create polygon
plgn <- st_polygon(list(cbind(c(5, 10, 8, 2, 3, 5), c(2, 1, 7, 9, 5, 2))))

# Get occurrence points
occurrences_sf <- simulate_occurrences(plgn)

# Create grid with bias weights
grid <- st_make_grid(
    plgn,
    n = c(10, 10),
    square = TRUE) \%>\%
  st_sf()
grid$bias_weight <- runif(nrow(grid), min = 0, max = 1)

# Calculate occurrence bias
occurrence_bias <- apply_manual_sampling_bias(occurrences_sf, grid)
occurrence_bias

# Visualise where the bias is
ggplot() +
  geom_sf(data = plgn) +
  geom_sf(data = grid, alpha = 0) +
  geom_sf(data = occurrence_bias, aes(colour = bias_weight)) +
  geom_sf_text(data = grid, aes(label = round(bias_weight, 2))) +
  theme_minimal()
}
\seealso{
Other detection: 
\code{\link{apply_polygon_sampling_bias}()}
}
\concept{detection}
