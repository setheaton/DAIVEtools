#' Combines multiple outcomes using weighted sums
#' @export
weight_outcomes <- function(data, outcomes, weights) {
  sum <- 0
  for (i in 1:length(outcomes)){
    col <- which(colnames(data) == outcomes[i])
    sum <- sum + (data[col] * weights[i])
  }
  return(sum)
}
