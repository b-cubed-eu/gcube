#' Simulate occurrences within a spatiotemporal scope
#'
#' The function simulates occurrences of a species within a given spatial
#' and/or temporal extend.
#'
#' @param df ...
#' @param ... ...
#'
#' @returns ...
#'
#' @export ...
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import assertthat
#'
#' @family multispecies
#'
#' @examples
#'

map_simulate_occurrences <- function(
    df,
    ...,
    unnest = TRUE) {
  ### Start checks
  # 1. Check input type and length
  # Check if df is a dataframe
  stopifnot("`df` must be a dataframe." = inherits(df, "data.frame"))

  # Check if aggregate is a logical vector of length 1
  stopifnot("`unnest` must be a logical vector of length 1." =
              assertthat::is.flag(unnest) && assertthat::noNA(unnest))


  out_df <- df %>%
    mutate(
      occurrences = purrr::pmap(
        dplyr::select(.,
                      "plgn",
                      "initial_average_abundance",
                      "n_time_points",
                      "temporal_function",
                      "sd_step",
                      "spatial_autocorr",
                      "seed"),
        simulate_occurrences))

  if (unnest) {
    out_df <- out_df %>%
      tidyr::unnest(cols = "occurrences")
  }

  return(out_df)
}
