% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_occurrences_from_raster.R
\name{sample_occurrences_from_raster}
\alias{sample_occurrences_from_raster}
\title{Sample occurrences from spatial random field}
\usage{
sample_occurrences_from_raster(rs, ts)
}
\arguments{
\item{rs}{a raster object (terra)}

\item{ts}{vector with the number of occurrences by time step}
}
\value{
An sf object with POINT geometry
}
\description{
Draws occurrences (points) from a spatial random field (raster)
}
\examples{
library(terra)
library(sf)

r <- rast(ncol = 50, nrow = 50, xmin = 0, xmax = 50, ymin = 0, ymax = 50)
values(r) <- 1:ncell(r)
timeseries <- c(20, 40, 60)

pts_occ <- sample_occurrences_from_raster(rs = r, ts = timeseries)

plot(r)
plot(pts_occ, add = TRUE, color = "black")

}