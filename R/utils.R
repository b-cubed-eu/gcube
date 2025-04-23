#' Function to collect arguments from cube simulation functions
#'
#' This function collects all arguments from a cube simulation function
#' (`simulate_occurrences()`, `sample_observations()`, `filter_observations()`,
#' `add_coordinate_uncertainty()`, or `grid_designation()`)
#' also taking into account the ellipsis (`...`) and `temporal_function`
#' of `simulate_occurrences()`.
#'
#' @param f One of five cube simulation functions: `simulate_occurrences()`,
#' `sample_observations()`, `filter_observations()`,
#' `add_coordinate_uncertainty()`, or `grid_designation()`.
#' @param df A dataframe containing multiple rows, each representing a
#' different species. The columns are function arguments with values used for
#' mapping `f` for each species. This dataframe is used to get the correct
#' arguments of the `temporal_function` of if `f` is `simulate_occurrences()`.
#'
#' @importFrom methods formalArgs
#'
#' @returns A character vector of argument names for the specified function `f`.
#' If `f` is `simulate_occurrences` and the dataframe `df` contains a
#' `temporal_function` column, the returned vector includes arguments from the
#' `temporal_function`.
#'
#' @noRd

get_function_arguments <- function(f, df) {
  # Get function arguments excl. ellipsis
  f_args_raw <- methods::formalArgs(f)
  f_args <- f_args_raw[f_args_raw != "..."]

  # Also get argument names of temporal_function if necessary
  if (identical(f, simulate_occurrences) &&
        "temporal_function" %in% colnames(df)) {
    temp_f_args <- sapply(df$temporal_function, function(f) {
      if (is.function(f)) methods::formalArgs(f)
    })
    f_args <- unique(c(f_args, unlist(temp_f_args)))
  }

  return(f_args)
}


#' Handle repeated warnings during mapping
#'
#' This function captures and reports warnings that occurred during the
#' execution of a mapping function wrapped with `purrr::quietly()`. The function
#' extracts warnings from the results stored in a list-column of a dataframe,
#' counts their occurrences, and prints a summary of these warnings.
#'
#' @param df A dataframe containing a list-column with the output of a mapped
#' function wrapped with `purrr::quietly()`. This list-column should include
#' a `$warnings` component for capturing warnings.
#' @param mapped_col The name of the list-column in `df` that contains the
#' output from the mapped function. Defaults to `"mapped_col"`.
#'
#' @returns A dataframe identical to the input `df`, but with the list-column
#' specified by `mapped_col` updated to retain only the results from the mapping
#' function, excluding any warnings.
#'
#' @details This function first verifies that the specified `mapped_col` is
#' present in the dataframe and that it is a list-column. It then collects all
#' warnings from the list-column, counts their occurrences, and prints a summary
#' of these warnings. Finally, the function updates the dataframe to include
#' only the results from the mapping function, discarding any warning
#' information.
#'
#' @noRd

handle_mapped_warnings <- function(df, mapped_col = "mapped_col") {
  ### Start checks
  stopifnot("`mapped_col` not present in provided dataframe." =
              mapped_col %in% colnames(df))
  stopifnot("`mapped_col` must be a list-column." =
              inherits(df[[mapped_col]], "list"))
  ### End checks

  # Collect and count warnings in dataframe
  warning_df <- data.frame(
    table(unlist(sapply(df[[mapped_col]], function(warn) warn$warnings)))
  )

  # Print warnings
  if (nrow(warning_df) > 0) {
    warning(paste(sum(warning_df$Freq), "warnings during mapping:"),
            call. = FALSE)
    for (i in seq_len(nrow(warning_df))) {
      message <- warning_df[i, "Var1"]
      count <- warning_df[i, "Freq"]
      warning(paste0(message, " [", count, " times]"),
              call. = FALSE)
    }
  }

  # Only retain result from mapping
  df$mapped_col <- lapply(df[[mapped_col]], function(val) val$result)

  return(df)
}


#' Convert Meters to Degrees Based on Latitude
#'
#' Converts a distance in meters to approximate degrees of latitude and
#' longitude, using the standard approximation that 1 degree of latitude is
#' approximately 111,320 meters. The longitude conversion accounts for the
#' cosine of the latitude (in degrees).
#'
#' @param meters Numeric. Distance in meters to be converted.
#' @param latitude Numeric. Latitude in degrees at which the conversion is made.
#'
#' @return A list with two elements:
#'   - `lat`: Approximate latitude offset in degrees.
#'   - `lon`: Approximate longitude offset in degrees, adjusted for the input
#'            latitude.
#'
#' @noRd

meters_to_degrees <- function(meters, latitude) {
  lat_degree <- meters / 111320
  lon_degree <- meters / (111320 * cos(latitude * pi / 180))
  return(list(lat = lat_degree, lon = lon_degree))
}
