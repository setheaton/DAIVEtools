##### DONE COMMENTING

#' Generates effect codes for each intervention version using component labels.
#'
#' @description
#' This function generates effect codes using a list of component names. Effect
#' codes will include an intercept, main effects, and interaction effects up to
#' the k-way interaction for k components.
#' @param components A vector containing a char value with the names of each
#' component.
#' @param ix The limit of n-term interactions that should be included
#' @return A data frame with effect codes.
#' @export
#' @examples
#' get_codes(c("A", "B", "C", "D"))
get_codes <- function(components, ix) {
  # first, create codes df
  codes <- make_codes_frame(components)

  # iterate through all possible lengths of combinations of components up to
  # interactions limit
  for(i in 2:ix) {
    # get combinations of components of that length
    combinations <- utils::combn(components, i)
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

' Standardize column names to the "b_componentA:componentB" pattern
#'
#' Columns already following the pattern are kept as-is (component order is
#' normalized to the order in `components`). Other columns whose names can be
#' parsed as a combination of component labels (or an intercept) are renamed.
#' Anything that can't be interpreted is treated as an "erroneous" column and
#' is kept or dropped depending on `other_cols`.
#'
#' @param df         A data.frame.
#' @param components Character vector of component labels.
#' @param prefix     Prefix for expected names (default "b_").
#' @param sep        Separator used in the expected names (default ":").
#' @param intercept  Label for the intercept (default "Intercept").
#' @param draw_col   Optional single column name to relabel as ".draw"
#'                   (e.g. "Iteration"). NULL (default) does no relabeling.
#' @param other_cols "keep" (default) or "drop" columns that don't match.
#' @param verbose    Report renames / skipped columns via message().
#' @return A data.frame with standardized names.
#' @export
standardize_colnames <- function(df,
                                 components,
                                 prefix = "b_",
                                 sep = ":",
                                 intercept = "Intercept",
                                 draw_col = NULL,
                                 other_cols = c("keep", "drop"),
                                 verbose = TRUE) {
  other_cols <- match.arg(other_cols)
  stopifnot(is.data.frame(df),
            is.character(components),
            !anyDuplicated(components),
            is.null(draw_col) || (is.character(draw_col) && length(draw_col) == 1))

  # Relabel the requested column as ".draw" (before anything else is processed)
  if (!is.null(draw_col)) {
    idx <- which(names(df) == draw_col)
    if (length(idx) == 0) {
      warning("draw_col '", draw_col, "' not found in df.", call. = FALSE)
    } else if (".draw" %in% names(df) && draw_col != ".draw") {
      warning("df already has a '.draw' column; '", draw_col,
              "' was not relabeled.", call. = FALSE)
    } else {
      names(df)[idx] <- ".draw"
      if (verbose && draw_col != ".draw") {
        message("Renamed: ", draw_col, " -> .draw")
      }
    }
  }

  # Separators that might appear between components in non-standard names.
  # The empty string allows concatenated labels (e.g. "AB" -> A, B).
  in_seps <- unique(c(sep, ":", ".", "_", "*", "-", " ", ""))
  # Try longer labels first so "AB" isn't mistaken for "A" + "B..."
  comps_by_len <- components[order(-nchar(components))]

  # Recursively segment a string into component labels separated by in_seps.
  # Handles labels that themselves contain dots/underscores. Every match
  # consumes at least one character, so the recursion always terminates.
  parse_name <- function(s) {
    for (cmp in comps_by_len) {
      if (startsWith(s, cmp)) {
        rest <- substring(s, nchar(cmp) + 1)
        if (!nzchar(rest)) return(cmp)
        for (sp in in_seps) {
          if (startsWith(rest, sp)) {
            tail_parts <- parse_name(substring(rest, nchar(sp) + 1))
            if (!is.null(tail_parts)) return(c(cmp, tail_parts))
          }
        }
      }
    }
    NULL
  }

  # Map one column name to its canonical form (NA if not interpretable)
  canonical <- function(nm) {
    if (nm == ".draw") return(NA_character_)
    candidates <- nm
    if (startsWith(nm, prefix)) {
      candidates <- c(substring(nm, nchar(prefix) + 1), nm)
    }
    for (base in candidates) {
      # Intercept variants: "Intercept", "(Intercept)", "intercept"
      if (tolower(gsub("[()]", "", base)) == tolower(intercept)) {
        return(paste0(prefix, intercept))
      }
      parts <- parse_name(base)
      if (!is.null(parts) && !anyDuplicated(parts)) {
        ord <- components[sort(match(parts, components))]
        return(paste0(prefix, paste(ord, collapse = sep)))
      }
    }
    NA_character_
  }

  old <- names(df)
  new <- vapply(old, canonical, character(1), USE.NAMES = FALSE)
  matched <- !is.na(new)
  already_ok <- matched & new == old

  # Guard against collisions: two columns mapping to the same target, or a
  # rename onto a name that already exists as a correct column.
  needs_rename <- matched & !already_ok
  target_counts <- table(new[matched])
  collides <- needs_rename &
    (new %in% old[already_ok] | new %in% names(target_counts)[target_counts > 1])
  if (any(collides) && verbose) {
    warning("Skipped renaming due to name collisions: ",
            paste(old[collides], collapse = ", "), call. = FALSE)
  }
  needs_rename <- needs_rename & !collides
  matched <- already_ok | needs_rename

  names(df)[needs_rename] <- new[needs_rename]

  if (verbose) {
    if (any(needs_rename)) {
      message("Renamed: ",
              paste0(old[needs_rename], " -> ", new[needs_rename], collapse = "; "))
    }
    if (!any(matched)) {
      warning("No columns matched any combination of the supplied components.",
              call. = FALSE)
    }
    if (any(!matched)) {
      message(ifelse(other_cols == "drop", "Dropped", "Left unchanged"),
              " unmatched columns: ", paste(old[!matched], collapse = ", "))
    }
  }

  if (other_cols == "drop") df <- df[, matched, drop = FALSE]
  df
}

# ---- Example -----------------------------------------------------------
# components <- c("A", "B", "C")
# df <- data.frame(Iteration = 1, Intercept = 1, A = 2, `B.A` = 3, AC = 4,
#                  junk = 5, check.names = FALSE)
# standardize_colnames(df, components, draw_col = "Iteration")
# #> names: .draw, b_Intercept, b_A, b_A:B, b_A:C, junk
