#' Function to collect arguments of cube simulation functions
#'
#' This function collects all arguments of a cube simulation function
#' (`simulate_occurrences()`, `sample_observations()` or `grid_designation()`)
#' also taking into account the ellipsis (`...`) and `temporal_function`
#' of `simulate_occurrences()`.
#'
#' @param f One of three cube simulation functions: `simulate_occurrences()`,
#' `sample_observations()` or `grid_designation()`.
#' @param df A dataframe containing multiple rows. Each row is considered a
#' different species. The columns are function arguments with values used for
#' mapping `simulate_occurrences()` for each species. `df` can have columns that
#' are not used by this function. They will be retained in the output.
#'
#' @importFrom methods formalArgs
#'
#' @noRd

get_function_arguments <- function(f, df) {
  # Get function arguments excl. ellipsis
  f_args_raw <- methods::formalArgs(f)
  f_args <- f_args_raw[f_args_raw != "..."]

  # Also get argument names of temporal_function if necessary
  if (identical(f, simulate_occurrences)) {
    temp_f_args <- sapply(df$temporal_function, function(f) {
      if (is.function(f)) methods::formalArgs(f)
    })
    f_args <- unique(c(f_args, unlist(temp_f_args)))
  }

  return(f_args)
}
