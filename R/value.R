##### DONE COMMENTEING

#' Generates a table of expected value for all possible combinations of
#' components.
#' @description
#' This function "("get expected value") updates a daive_grid object to include
#' a summary dataframe with estimated performance for each alternative and a
#' predicted_outcomes dataframe formatted to estimate predicted alternative
#' performance. The summary frame can be used to calculate a frontier and create
#' DAIVE plots.
#' @param g A daive_grid object
#' @return A daive_grid object
#' @import data.table
#' @export
get_ev <- function(g) {
  # take a vector of bayesian models as a parameter
  # take a vector of component column names
  settings <- g$settings


  # get settings from grid object
  k <- settings$k
  separator <- settings$separator

  # get draws, codes, outcomes, and components from grid object
  draws <- g$data
  codes <- g$codes
  outs <- g$outcomes
  components <- g$components

  # set up outcomes frame
  if (length(draws) > 1 && inherits(draws, "list")) {
    # if multiple sets of draws are included in the daive_grid object
    outcomes <- get_outcomes_asframe(draws[[1]], codes, k)
    colnames(outcomes) <- append(components, outs[1])
    for (i in 2:length(draws)) {
      outcomes$tmp <- get_outcomes_asframe(draws[[i]], codes, k)$out
      colnames(outcomes) <- append(utils::head(colnames(outcomes), -1), outs[i])
    }
  } else {
    # if only one set of draws is included in the daive_grid object
    outcomes <- get_outcomes_asframe(draws, codes, k)
    colnames(outcomes) <- append(components, outs)
  }

  # if scale has the default value
  if(identical(settings$scale, "0-1")) {
    # pass NULL to helper function (to use default scale function)
    outcomes <- scale_outcomes(outcomes, outs, ".scale", NULL)
  } else if(identical(settings$scale, "z_score")) {
    # pass custom z helper function if specified
    outcomes <- scale_outcomes(outcomes, outs, ".scale", scale_z)
  } else {
    # otherwise pass custom scale argument
    outcomes <- scale_outcomes(outcomes, outs, ".scale", settings$scale)
  }

  # update the grid object with predicted_outcomes
  g$predicted_outcomes <- outcomes

  # initialize value_summary dataframe
  value_summary <- codes[,2:(2 + k - 1)]
  value_summary <- get_alternatives_names(value_summary, components,
                                          sep=separator)

  # column labels for outcomes
  outcome_colnames <- paste0(outs, ".scale")
  outcome_colnames <- c(outs, outcome_colnames)

  # set as data tables
  data.table::setDT(outcomes)
  data.table::setDT(value_summary)

  # calculate value summary
  summary_dt <- outcomes[, lapply(.SD, mean), by = components,
                         .SDcols = outcome_colnames]

  # convert back to data.frame and combine with value_summary
  summary_df <- as.data.frame(summary_dt)
  value_summary <- as.data.frame(value_summary)
  value_summary <- merge(value_summary, summary_df, by=components)

  # if costs equals default, set costs to one
  if(identical(g$costs, "default")) {
    g$costs <- rep(1, times=k)
  }

  # add costs to value_summary
  costs_frame <- build_cost_grid(components, g$costs)
  value_summary <- merge(value_summary, costs_frame, by=components)

  # update grid object with expected values table and new settings
  g$ev_table <- value_summary

  # update grid object with new alternatives names and settings
  settings$alternatives <- value_summary$names
  g$settings <- settings

  # prepare ev draws
  outcomes <- as.data.frame(outcomes)
  outcomes <- get_alternatives_names(outcomes, components, separator)
  outcomes_draws <- lapply(outcome_colnames, flip_by_names, df=outcomes,
                           vec=settings$alternatives)

  # save ev draws to g object
  g$ev_draws <- outcomes_draws

  # update weights
  g <- update_weights(g, settings$weights)

  # return updated grid object
  g
}

# helper function to get outcomes as a dataframe from draws
get_outcomes_asframe <- function(draws, codes, k) {
  # Ensure draws are ordered by .draw so row order matches rep() below
  draws <- draws[order(draws$.draw), ]

  # One matrix: rows = draws, columns = parameters (drop .draw column)
  draw_matrix <- as.matrix(draws[, setdiff(names(draws), ".draw")])

  # (n_codes x p) %*% (p x n_draws) = n_codes x n_draws
  outcome_matrix <- as.matrix(codes) %*% t(draw_matrix)

  n_draws <- nrow(draw_matrix)

  # set effects codes to repeat for the length of the outcomes frame
  params <- list()
  for (i in 1:k) {
    comp_name <- colnames(codes)[i + 1]
    params[[comp_name]] <- rep(codes[[comp_name]], n_draws)
  }
  params[["out"]] <- as.vector(outcome_matrix)

  # return the outcomes vector and codes as a data frame
  do.call(data.frame, params)
}

# helper function to add intervention names to codes frame
get_alternatives_names <- function(frame, components, sep) {
  # get number of components
  k <- length(components)

  # initialize names vector
  names.vec <- c()
  # generate alternatives names for each of 2^k possible combinations
  for (i in 1:2^k) {
    string <- ""
    if (sum(frame[i, 1:k]) == -1*k) {
      # manually set all off
      string = "All Off"
    } else {
      # otherwise, combine component names with separator
      for (j in 1:k){
        if(frame[i, j] == 1) {
          string <- paste0(string, sep, components[j])
        }
      }
      # strip the superfluous leading separator if the separator is not empty
      if(sep != "") {
        expression <- paste0("^.{", nchar(sep), "}")
        string <- gsub(expression, '', string)
      }
    }
    # append the label to the vector
    names.vec <- append(names.vec, string)
  }
  # add the vector to the frame
  frame$names <- names.vec
  frame
}

# helper function
scale_outcomes <- function(outcomes, cols_to_scale, scaled_label, scale) {
  unknown <- setdiff(names(scale), c(cols_to_scale, ".default"))
  if (length(unknown) > 0) {
    warning("Ignoring scale entries for unrecognized columns: ",
            paste(unknown, collapse = ", "))
  }

  # Resolve which function applies to a given column
  scale_fn_for <- function(col) {
    if (is.null(scale)) {
      scale01
    } else if (is.function(scale)) {
      scale                               # if single fn, apply to all columns
    } else if (is.list(scale)) {
      scale[[col]] %||% scale[[".default"]] %||% scale01
    } else {
      stop("`scale` must be NULL, a function, or a named list of functions.")
    }
  }

  for (col in cols_to_scale) {
    fn <- scale_fn_for(col)
    outcomes[[paste0(col, scaled_label)]] <- fn(outcomes[[col]])
  }

  outcomes
}

# helper function for default scaling
scale01 <- function(col) {
  min <- min(col)
  max <- max(col)

  # rescale from 0 to 1
  scaled_col <- (col - min)/(max-min)
  # make sure the minimum and maximum are equal to 0 and 1 (to correct rounding)
  scaled_col[scaled_col < 0] <- 0
  scaled_col[scaled_col > 1] <- 1

  scaled_col
}

# helper function for z score scaling
scale_z <- function(col) {
  (col - mean(col))/sd(col)
}

# flip draws by names
flip_by_names <- function(df, vec, label) {
  # each alternative name
  for(i in 1:length(vec)) {
    # subset df$"label" column with rows for that alternative
    add.vec <- df[[label]][df$names == vec[i]]
    # if first pass
    if (i == 1) {
      # create a data frame from that vector
      result <- data.frame(add.vec)
    } else {
      # append vector to data frame
      result <- cbind(result, add.vec)
    }
  }
  # update colnames to reflect alternatives names
  colnames(result) <- vec
  result
}

# helper func to build costs grid
build_cost_grid <- function(components, costs) {
  # check for one cost value per component
  stopifnot(length(components) == length(costs))

  # define two possible levels (-1 for off and 1 for on) for each component and
  # create a full factorial grid
  levels_args <- setNames(
    lapply(components, function(x) c(-1, 1)),
    components
  )
  levels_grid <- do.call(expand.grid, levels_args)

  # convert levels from -1/1 -> 0/1 "on/off" indicator and calculate costs
  on_indicator <- (as.matrix(levels_grid) + 1) / 2
  levels_grid$cost <- as.numeric(on_indicator %*% costs)

  levels_grid
}
