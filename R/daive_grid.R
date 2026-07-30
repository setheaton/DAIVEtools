# Constructor
new_daive_grid <- function(data, outcomes, components, settings) {
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

# Final checks validator
check_daive_grid <- function(x) {
  # check all required keys
  required_keys = c("data", "outcomes", "components", "codes", "settings")
  if(!all(required_keys %in% names(x))){
    stop("grid is missing required key", call. = FALSE)
  }

  if (!is.list(x$settings)) stop("settings must be a list", call. = FALSE)
  x
}

# helper
check_components <- function(x) {
  # --- type check ---
  if (!is.character(x)) {
    stop("`components` must be a character string or character vector, got `",
         class(x)[1], "`.", call. = FALSE)
  }

  # --- non-empty check ---
  if (length(x) == 0) {
    stop("`components` must contain at least one component name.", call. = FALSE)
  }

  # --- no missing / blank strings ---
  if (anyNA(x) || any(trimws(x) == "")) {
    stop("`components` cannot contain NA or empty-string values.", call. = FALSE)
  }

  # --- no duplicates ---
  if (anyDuplicated(x) > 0) {
    stop("`components` contains duplicate name(s): ",
         paste(unique(x[duplicated(x)]), collapse = ", "),
         call. = FALSE)
  }
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

# helper
check_draws <- function(data, draws_labels) {
  if(!all(draws_labels %in% colnames(data))) {
    stop("`data` column names do not match draws labels for `components`")
  }
  data[, intersect(draws_labels, colnames(data)), drop = FALSE]
}

# helper
check_outcomes <- function(x, data) {
  # --- type check ---
  if (!is.character(x)) {
    stop("`outcomes` must be a character string or character vector, got `",
         class(x)[1], "`.", call. = FALSE)
  }

  # --- non-empty check ---
  if (length(x) == 0) {
    stop("`outcomes` must contain at least one component name.", call. = FALSE)
  }

  # --- no missing / blank strings ---
  if (anyNA(x) || any(trimws(x) == "")) {
    stop("`outcomes` cannot contain NA or empty-string values.", call. = FALSE)
  }

  # --- no duplicates ---
  if (anyDuplicated(x) > 0) {
    stop("`outcomes` contains duplicate name(s): ",
         paste(unique(x[duplicated(x)]), collapse = ", "),
         call. = FALSE)
  }

  # --- correct length ---
  if (length(x) != length(x)) {
    stop("`outcomes` contains a different number of values than the provided nubmer of draws")
  }
}

#' @export
# User-facing helper (what people actually call)
daive_grid <- function(data, outcomes, components, separator="", weights=NA) {
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

  # create list of settings
  settings = list(
    "separator" = separator,
    "k" = length(components),
    "draws_labels" = draws_labels,
    "weights" = weights,
    "alternatives" = NULL
  )

  # final checks and return the object
  g <- check_daive_grid(new_daive_grid(data, outcomes, components, settings))
  get_ev(g)
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
  return(NA)
}
