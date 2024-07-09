#' Simulate occurrences within a spatiotemporal scope
#'
#' The function simulates occurrences of a species within a given spatial
#' and/or temporal extend.
#'
#' @param df ...
#' @param unnest Logical. If `TRUE` (default), ... Otherwise ...
#' @param arg_list ...
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
    unnest = TRUE,
    arg_list = NA) {
  ### Start checks
  # 1. Check input type and length
  # Check if df is a dataframe
  stopifnot("`df` must be a dataframe." = inherits(df, "data.frame"))

  # Check if aggregate is a logical vector of length 1
  stopifnot("`unnest` must be a logical vector of length 1." =
              assertthat::is.flag(unnest) && assertthat::noNA(unnest))

  ### End checks

  sim_occ_args <- formalArgs(simulate_occurrences)
  sim_occ_names <- sim_occ_args[sim_occ_args != "..."]

  temp_f_args <- sapply(df$temporal_function, function(f) {
    if (is.function(f)) formalArgs(f)
  })
  temp_f_names <- unique(unlist(temp_f_args))

  col_arg_names <- unique(c(sim_occ_names, temp_f_names))

  if (!is.na(arg_list)) {
    df <- df %>%
      rename(!!!arg_list)
  }

  selection_names <- intersect(names(df), col_arg_names)

  out_df <- df %>%
    mutate(
      occurrences = purrr::pmap(
        dplyr::select(., all_of(selection_names)),
        simulate_occurrences)
    )

  if (unnest) {
    out_df <- out_df %>%
      tidyr::unnest(cols = "occurrences")
  }

  return(out_df)
}
