#' Generates effect codes for each intervention version using component labels.
#'
#' @description
#' This function generates effect codes using a list of component names. Effect
#' codes will include an intercept, main effects, and interaction effects up to
#' the k-way interaction for k components.
#' @param components A vector containing a char value with the names of each
#' component.
#' @return A dataframe with effect codes.
#' @export
#' @examples
#' get_codes(c("A", "B", "C", "D"))
get_codes <- function(components) {
  # To-DO: Add safety guardrails (with an optional flag keyword)

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
  # DEBUG NOTE: THIS COLUMN IS NOT NUMERIC WHICH CAUSES AN ISSUE LATER
  params <- list(list(c(1)))

  # for each element of components, add an argument for the call to expand.grid
  for (i in 1:length(components)) {
    params <- append(params, list(c(-1, 1)))
  }

  # add intercept label to the beginning of component labels
  components <- append("Intercept", components)

  # add component labels as name to the params list
  names(params) <- components

  # call expand grid and return
  codes <- do.call(expand.grid, params)

  # manually make codes cast the intercept column as numeric, return
  codes$Intercept <- as.numeric(codes$Intercept)
  return(codes)
}


# helper func to multiply columns together
multiply_columns <- function(df, col_names) {
  new_col_name <- paste(col_names, collapse = "x")
  df[[new_col_name]] <- Reduce("*", df[col_names])
  return(df)
}
