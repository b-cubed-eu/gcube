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


#' Function to return repeated warnings during mapping
#'
#' This function stores all warnings captured by `purrr::quietly()` that
#' occurred during mapping and reports them in a clean way.
#'
#' @param df A dataframe containing a list-column with the output of a mapped
#' function wrapped with `purrr::quietly()`.
#' @param mapped_col The name of the list-column described above. Defaults to
#' `"mapped_col"`.
#'
#' @noRd

handle_mapped_warnings <- function(df, mapped_col = "mapped_col") {
  ### Start checks
  stopifnot("`mapped_col` not present in provided dataframe." =
              mapped_col %in% colnames(df))
  stopifnot("`mapped_col` must be a list-column." =
              inherits(df[, mapped_col], "list"))
  ### End checks

  # Collect and count warnings in dataframe
  warning_df <- data.frame(
    table(unlist(sapply(df[, mapped_col], function(warn) warn$warnings)))
  )

  # Print warnings
  if (nrow(warning_df) > 0) {
    message(paste(sum(warning_df$Freq), "warnings during mapping:"))
    for (i in seq_len(nrow(warning_df))) {
      message <- warning_df[i, "Var1"]
      count <- warning_df[i, "Freq"]
      warning(paste0(message, " [", count, " times]"))
    }
  }

  # Only retain result from mapping
  df$mapped_col = sapply(df[, mapped_col], function(val) val$result)

  return(df)
}
