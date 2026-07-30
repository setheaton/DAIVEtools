#' Combines multiple outcomes using weighted sums.
#'
#' @description
#' This function takes a data frame, a vector of outcome labels, and a set of
#' numeric weights and generates a vector of combined outcome values using
#' simple additive weighting.
#' @param grid_object A daive_grid object
#' @param weights A vector of numeric values with weights for each outcome
#' @return A vector containing the weighted sums of the specified columns for
#' each row of data.
#' @export
weight_outcomes <- function(grid_object, weights) {
  g <- grid_object
  data <- g$ev_table
  outs <- g$outcomes
  outs <- paste0(outs, ".scale")
  sum <- 0

  weighted_cols <- lapply(seq_along(outs), function(i) {
    col <- which(colnames(data) == outs[i])
    data[[col]] * weights[i]
  })

  sum <- Reduce(`+`, weighted_cols)
  data$value_function <- sum
  g$ev_table <- data
  g$settings$weights <- weights
  g
}
