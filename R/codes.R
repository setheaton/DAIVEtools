#' Generate effect codes for each intervention version using component labels
#' @export
get_codes <- function(components) {
  # first, create codes df
  codes <- make_codes_frame(components)

  # iterate through all possible lengths of combinations
  for(i in 2:length(components)) {
    # get combinations of components of that length
    combinations <- combn(components, i)
    for(i in 1:ncol(combinations)) {
      codes <- multiply_columns(codes, combinations[,i])
    }
  }
  return(codes)
}

# helper func to create the codes df
make_codes_frame <- function(components) {
  # make list to hold parameter vectors, add vector for intercept
  params <- list(list(c(1)))

  # for each element of components, add an argument for the call to expand.grid
  for (i in 1:length(components)) {
    params <- append(params, list(c(-1, 1)))
  }

  # add intercept label to the beginning of component labels
  components <- append("Intercept", components)

  # add component lables as name to the params list
  names(params) <- components

  # call expand grid and return
  codes <- do.call(expand.grid, params)
  return(codes)
}


# helper func to multiply columns together
multiply_columns <- function(df, col_names) {
  new_col_name <- paste(col_names, collapse = "x")
  df[[new_col_name]] <- Reduce("*", df[col_names])
  return(df)
}
