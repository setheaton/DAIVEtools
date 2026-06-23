#' Combines multiple outcomes using weighted sums.
#'
#' @description
#' This function takes a data frame, a vector of outcome labels, and a set of
#' numeric weights and generates a vector of combined outcome values using
#' simple additive weighting.
#' @param data A data frame containing data for multiple outcomes
#' @param outs A vector of char values with column labels for each outcome
#' @param weights A vector of numeric values with weights for each outcome
#' @return A vector containing the weighted sums of the specified columns for
#' each row of data.
#' @export
weight_outcomes <- function(data, outs, weights) {
  sum <- 0
  for (i in 1:length(outs)){
    col <- which(colnames(data) == outs[i])
    sum <- sum + (data[col] * weights[i])
  }

  return(sum[,1])
}
