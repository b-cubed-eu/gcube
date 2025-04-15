#' Filter detected occurrences
#'
#' This function filters observations from all occurrences based on the
#' `observed` column, typically created by the `sample_observations()`
#' function.
#'
#' @param observations_total An sf object with POINT geometry or a simple
#' dataframe with `observed` column containing logical values (`TRUE`/`FALSE`).
#' This format is typically created by the `sample_observations()` function.
#' @param invert Logical. If `FALSE` (default), the function filters to retain
#' only observed occurrences. If `TRUE`, it filters out unobserved
#' occurrences.
#'
#' @returns A data frame or an sf object with POINT geometry containing the
#' filtered observations. If `invert = FALSE`, the function returns observed
#' occurrences. If `invert = TRUE`, it returns unobserved occurrences.
#'
#' @export
#'
#' @import assertthat
#'
#' @family main
#'
#' @examples
#' # Create dataframe with sampling status column
#' occurrences_data <- data.frame(
#'     time_point = 1,
#'     sampling_prob = seq(0.5, 1, 0.1),
#'     observed = rep(c(FALSE, TRUE), each = 3)
#'   )
#'
#' # Keep detected occurrences
#' filter_observations(occurrences_data)
#'
#' # Keep undetected occurrences
#' filter_observations(occurrences_data, invert = TRUE)

filter_observations <- function(observations_total, invert = FALSE) {
  ### Start checks
  # Check if observations_total is a dataframe and/or an sf object
  stopifnot("`observations_total` must be an sf object or a dataframe." =
              inherits(observations_total, "sf") ||
              inherits(observations_total, "data.frame"))

  # Check if invert is a logical vector of length 1
  stopifnot("`invert` must be a logical vector of length 1." =
              assertthat::is.flag(invert) && assertthat::noNA(invert))
  ### End checks


  # Filter dataframe
  if (invert) {
    observations_total[observations_total$observed != TRUE, ]
  } else {
    observations_total[observations_total$observed == TRUE, ]
  }
}
