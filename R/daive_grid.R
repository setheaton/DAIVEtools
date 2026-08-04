#' @export
# User-facing helper (what people actually call)
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

# Methods
#' @export
print.daive_grid <- function(x) {
  cat("<daive_grid object>\n")
  cat("Outcomes:", paste(x$outcomes, collapse = "; "), "\n")
  cat("Components:", paste(x$components, collapse = ", "), "\n")
  cat("Main and Interaction Effect Draws:  ", length(x$data), "draws objects:\n")
  print(lapply(x$data, head))
  cat("Outcomes Draws: \n")
  print(head(x$predicted_outcomes))
  cat("Expected Values Table: \n")
  print(head(x$ev_table))
  cat("Alternative Performance Draws: \n")
  print(lapply(x$ev_draws, head))
  invisible(x)   # invisible() so it doesn't double-print at the console
}

#' @export
summary.daive_grid <- function(object, ...) {
  # OTHER BIG TO-DO
  return(NA)
}
