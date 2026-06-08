#' Converts a list of names to col labels for spread_draws() output
#' @export
draws_labels <- function(names) {
  # Add colon between characters and b_ prefix (except for empty alternative)
  results <- ifelse(
    names == "All Off",
    "b_Intercept:", # include the trailing colon so it is removed later
    gsub("^", "b_", gsub("(.)", "\\1:", names))
  )

  # Remove the trailing colon left at the end of each string
  results <- gsub(":$", "", results)

  return(results)
}
