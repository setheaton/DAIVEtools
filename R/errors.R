# check if a set of colnames is included in a dataframe
check_colnames <- function(frame, cols) {
  all(cols %in% colnames(frame))
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
