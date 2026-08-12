#' DAIVE grid
#'
#' @param data A list of draws objects.
#' @param outcomes A char (or vector of chars) with name(s) of the outcome(s).
#' @param components A char (or vector of chars) with name(s) of the component(s).
#' @param costs A numeric or ordered char of numerics of component delivery costs.
#' @param separator A char to separate component names in alternatives labels.
#' @param scale A function used to scale outcomes for comparison.
#' @param weights A numeric or ordered vector of numerics for outcome weights.
#'
#' @returns A daive_grid object.
#' @export
daive_grid <- function(data, outcomes, components, costs="default", separator="",
                       scale="0-1", weights="default") {
  # validate components labels
  components <- as.character(components)
  check_components(components)

  # validate data input and clean it
  draws_labels <- append("b_Intercept", build_draws_labels(components))
  draws_labels <- append(".draw", draws_labels)
  if(inherits(data, "data.frame")) {
    data <- check_draws(data, draws_labels)
  } else if (inherits(data, "list")) {
    data <- lapply(data, check_draws, draws_labels=draws_labels)
  } else {
    stop("data must be a data frame or list")
  }

  # validate outcomes labels
  check_outcomes(outcomes, data)

  # set default weights
  if (identical(weights, "default")) {
    denom = 100*length(outcomes)
    weights <- rep(100/denom, times=length(outcomes))
  }

  # create list of settings
  settings = list(
    "separator" = separator,
    "k" = length(components),
    "draws_labels" = draws_labels,
    "weights" = weights,
    "scale" = scale,
    "alternatives" = NULL
  )

  # final checks and return the object
  g <- check_daive_grid(new_daive_grid(data, outcomes, components, costs, settings))
  get_ev(g)
}

# Constructor
new_daive_grid <- function(data, outcomes, components, costs, settings) {
  # generate codes
  codes <- get_codes(components)

  structure(
    list(
      data = data,
      outcomes = outcomes,
      components = components,
      codes = codes,
      costs = costs,
      predicted_outcomes = NULL,
      ev_table = NULL,
      ev_draws = NULL,
      settings = settings
    ),
    class = "daive_grid"
  )
}

# helper
build_draws_labels <- function(components) {
  n <- length(components)

  # Generate all subsets of every size, from single terms to full interaction
  all_terms <- unlist(lapply(seq_len(n), function(k) {
    combos <- combn(components, k, simplify = FALSE)
    vapply(combos, paste, character(1), collapse = ":")
  }))

  paste0("b_", all_terms)
}

#' Get draws values.
#'
#' @param g A daive_grid object.
#'
#' @returns A dataframe or list of draws contained in the input.
#' @export
get_draws <- function(g) {
  g$data
}

#' Get expected value table.
#'
#' @param g A daive_grid object.
#'
#' @returns A dataframe with the expected values table contained in the input.
#' @export
get_table <- function(g) {
  g$ev_table
}

#' Get predicted outcomes.
#'
#' @param g A daive_grid object.
#'
#' @returns A dataframe with the predicted values contained in the input.
#' @export
get_predicted_outcomes <- function(g) {
  g$predicted_outcomes
}

#' Print
#'
#' @param g A daive_grid object.
#'
#' @returns Printed output with contents of the input.
#' @export
print.daive_grid <- function(g) {
  cat("<daive_grid object>\n")
  cat("Outcomes:", paste(g$outcomes, collapse = "; "), "\n")
  cat("Components:", paste(g$components, collapse = ", "), "\n")
  cat("Main and Interaction Effect Draws:  ", length(g$data), "draws objects:\n")
  print(lapply(g$data, head))
  cat("Outcomes Draws: \n")
  print(head(g$predicted_outcomes))
  cat("Expected Values Table: \n")
  print(head(g$ev_table))
  cat("Alternative Performance Draws: \n")
  print(lapply(g$ev_draws, head))
  invisible(g)   # invisible() so it doesn't double-print at the console
}

#' Summary
#'
#' @param g A daive_grid object.
#'
#' @returns Printed output describing expected value and the frontier.
#' @export
summary.daive_grid <- function(g) {
  # OTHER BIG TO-DO
  cat("DAIVE grid object with", length(g$outcomes), "outcomes and", length(g$components), "components.\n")
  cat("Outcomes:", paste(g$outcomes, collapse = "; "), "\n")
  cat("Components:", paste(g$components, collapse = ", "), "\n")
  cat("Expected Values Table: \n")
  print(g$ev_table)

  if(is.null(g$outcomes) || is.null(g$settings$weights)) {
    vfunc <- paste("V =", g$outcomes)
  } else {
    vfunc <- "V = "
    for(i in 1:length(g$outcomes)) {
      vfunc <- paste(vfunc, round(g$settings$weights[i], digits=3), " * (",
                     g$outcomes[i], ")", sep = "")
      if(i < length(g$outcomes)) {
        vfunc <- paste0(vfunc, " + ")
      }
    }
  }

  cat("Value Function:", vfunc, "\n\n")
  f <- frontier(g$ev_table, "value_function", "cost")
  cat("Alternatives on the frontier:", paste(f$names, collapse=", "))
}
