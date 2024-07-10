get_function_arguments <- function(f, df) {
  # Get function arguments excl. ellipsis
  f_args_raw <- formalArgs(f)
  f_args <- f_args_raw[f_args_raw != "..."]

  # Also get argument names of temporal_function if necessary
  if (identical(f, simulate_occurrences)) {
    temp_f_args <- sapply(df$temporal_function, function(f) {
      if (is.function(f)) formalArgs(f)
    })
    f_args <- unique(c(f_args, unlist(temp_f_args)))
  }

  return(f_args)
}
