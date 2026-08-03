#' Generates a table of expected value for all possible combinations of
#' components.
#' @description
#' This function creates a summary dataframe with estimated performance from
#' each alternative from  a list of posterior draws (as output from
#' tidybayes::spread_draws), outcome names, component names, and effects codes.
#' The summary frame can be used to calculate a frontier and create DAIVE plots.
#' @param draws A list object with posterior draws objects for each outcomes
#' outputted from tidybayes::spreadraws
#' @param outs A vector of char values with column labels for each outcome
#' @param components A vector of char values with labels for each component
#' @param codes A dataframe containing effect codes for component main and
#' interaction effects
#' @param separator An optional char variable to specify how components should
#' be separated in alternative labels
#' @return A dataframe containing expected value for each alternative on each
#' outcome.
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
    outcomes <- get_outcomes_asframe(draws[[1]], codes, k)
    colnames(outcomes) <- append(components, outs[1])
    for (i in 2:length(draws)) {
      outcomes$tmp <- get_outcomes_asframe(draws[[i]], codes, k)$out
      colnames(outcomes) <- append(utils::head(colnames(outcomes), -1), outs[i])
    }
  } else {
    outcomes <- get_outcomes_asframe(draws, codes, k)
    colnames(outcomes) <- append(components, outs)
  }

  # check -> is this scaling the outcome draws or the main+ix effect draws?
  # this is scaling the outcome draws
  for (i in 1:length(outs)) {
    outcomes <- scale_outcome(outcomes, outs[i], paste0(outs[i], ".scale"))
  }

  # update the grid object with outcomes
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

  # # add costs to value_summary
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

  # return updated grid object
  g
}

# helper function to get outcomes as a dataframe from draws
get_outcomes_asframe <- function(draws, codes, k) {

  # Ensure draws are ordered by .draw so row order matches rep() below
  draws <- draws[order(draws$.draw), ]

  # One matrix: rows = draws, columns = parameters (drop .draw column)
  draw_matrix <- as.matrix(draws[, setdiff(names(draws), ".draw")])

  # Single big matrix multiply replaces the entire lapply loop:
  # (n_codes x p) %*% (p x n_draws) = n_codes x n_draws
  outcome_matrix <- as.matrix(codes) %*% t(draw_matrix)

  n_draws <- nrow(draw_matrix)

  params <- list()
  for (i in 1:k) {
    comp_name <- colnames(codes)[i + 1]
    params[[comp_name]] <- rep(codes[[comp_name]], n_draws)
  }
  params[["out"]] <- as.vector(outcome_matrix)

  do.call(data.frame, params)
}

# helper function to add intervention names to codes frame
get_alternatives_names <- function(frame, components, sep) {
  # get number of components
  k <- length(components)

  # initialize names vector
  names.vec <- c()
  for (i in 1:2^k) {
    string <- ""
    if (sum(frame[i, 1:k]) == -1*k) {
      string = "All Off"
    } else {
      for (j in 1:k){
        if(frame[i, j] == 1) {
          string <- paste0(string, sep, components[j])
        }
      }
      if(sep != "") {
        expression <- paste0("^.{", nchar(sep), "}")
        string <- gsub(expression, '', string)
      }
    }
    names.vec <- append(names.vec, string)
  }
  frame$names <- names.vec
  return(frame)
}

# helper func to scale an outcome column
scale_outcome <- function(outcomes, colname, scaled_colname) {
  # get min and max of column
  outcome_col <- outcomes[[colname]]
  min <- min(outcome_col)
  max <- max(outcome_col)

  # scale the column
  scaled_col <- (outcome_col - min)/(max-min)
  scaled_col[scaled_col < 0] <- 0
  scaled_col[scaled_col > 1] <- 1
  outcomes[[scaled_colname]] <- scaled_col
  return(outcomes)
}

# flip draws by names
flip_by_names <- function(df, vec, label) {
  for(i in 1:length(vec)) {
    add.vec <- df[[label]][df$names == vec[i]]
    if (i == 1) {
      result <- data.frame(add.vec)
    } else {
      result <- cbind(result, add.vec)
    }
  }
  colnames(result) <- vec
  return(result)
}

# helper func to build costs grid
build_cost_grid <- function(components, costs) {
  stopifnot(length(components) == length(costs))

  levels_args <- setNames(
    lapply(components, function(x) c(-1, 1)),
    components
  )

  grid_args <- setNames(
    lapply(costs, function(c) c(0, c)),
    paste0(components, "_cost")
  )

  levels_grid <- do.call(expand.grid, levels_args)
  costs_grid  <- do.call(expand.grid, grid_args)

  levels_grid$cost <- rowSums(costs_grid)
  levels_grid
}
