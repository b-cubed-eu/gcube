% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_observations.R
\name{filter_observations}
\alias{filter_observations}
\title{Filter detected occurrences}
\usage{
filter_observations(observations_total, invert = FALSE)
}
\arguments{
\item{observations_total}{An sf object with POINT geometry or a simple
dataframe with \code{observed} column containing logical values (\code{TRUE}/\code{FALSE}).
This format is typically created by the \code{sample_observations()} function.}

\item{invert}{Logical. If \code{FALSE} (default), the function filters to retain
only observed occurrences. If \code{TRUE}, it filters out unobserved
occurrences.}
}
\value{
A data frame or an sf object with POINT geometry containing the
filtered observations. If \code{invert = FALSE}, the function returns observed
occurrences. If \code{invert = TRUE}, it returns unobserved occurrences.
}
\description{
This function filters observations from all occurrences based on the
\code{observed} column, typically created by the \code{sample_observations()}
function.
}
\examples{
# Create dataframe with sampling status column
occurrences_data <- data.frame(
    time_point = 1,
    sampling_prob = seq(0.5, 1, 0.1),
    observed = rep(c(FALSE, TRUE), each = 3)
  )

# Keep detected occurrences
filter_observations(occurrences_data)

# Keep undetected occurrences
filter_observations(occurrences_data, invert = TRUE)
}
\seealso{
Other main: 
\code{\link{add_coordinate_uncertainty}()},
\code{\link{grid_designation}()},
\code{\link{sample_observations}()},
\code{\link{simulate_occurrences}()},
\code{\link{virtualsample_to_sf}()}
}
\concept{main}
