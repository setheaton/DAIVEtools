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
#' @return A dataframe containing expected value for each alternative on each
#' outcome.
#' @export
values_table <- function(draws, outs, components, codes) {
  # take a vector of bayesian models as a parameter
  # take a vector of component column names

  # get number of components
  k <- length(components)

  # set up outcomes frame
  outcomes <- get_outcomes_asframe(draws[[1]], codes, k = k)
  colnames(outcomes) <- append(components, outs[1])

  if (length(draws) > 1) {
    for (i in 2:length(draws)) {
      outcomes$tmp <- get_outcomes_asframe(draws[[i]], codes, k = k)$out
      colnames(outcomes) <- append(head(colnames(outcomes), -1), outs[i])
    }
  }

  for (i in 1:length(outs)) {
    outcomes <- scale_outcome(outcomes, outs[i], paste0(outs[i], ".scale"))
  }

  value.summary <- codes[,2:(2 + k - 1)]
  value.summary <- get_alternatives_names(value.summary, components)

  # iterate through alternatives
  for (i in 1:2^k) {
    for (j in 1:length(outs)){
      # get subset of outcomes for that alternative
      subset <- plyr::match_df(outcomes, value.summary[i,1:k])
      value.summary[i, outs[j]] <- mean(subset[[outs[j]]])
      scale <- paste0(outs[j], ".scale")
      value.summary[i, scale] <- mean(subset[[scale]])
    }
  }

  return(value.summary)
}

#' Formats posterior draws to estimate alternative performance.
#' @export
prepare_draws <- function(draws, codes, components) {
  # format the outcome and scale it
  outcome <- get_outcomes_asframe(draws, codes, k = length(components))
  outcome <- scale_outcome(outcome, "out", "out")

  # add names for different alternatives
  outcome <- get_alternatives_names(outcome, components)

  # get a list of intervention names and "flip" the table
  names <- labels_draws(colnames(draws[4:length(draws)]))
  draws <- flip_by_names(outcome, names, "out")
  return(draws)
}


# helper function to get outcomes as a dataframe from draws
get_outcomes_asframe <- function(draws, codes, k=4) {

  # For a given draw index i, extract draw i from the posterior as a matrix and
  # left-multiply by codes to produce the outcome matrix for that draw
  out.list <- lapply(unique(draws$.draw), function(i) {
    draw_matrix <- draws |> subset(.draw == i) |>
      subset(select = c(4:length(draws))) |> as.matrix()
    as.matrix(codes) %*% t(draw_matrix)
  })

  # add outcomes data to a frame with effects codes and return
  params <- list()
  for(i in 1:k) {
    comp_name <- colnames(codes)[i + 1]
    params[[comp_name]] <- rep(c(codes[[comp_name]]), 4000)
  }
  params[["out"]] <- unlist(out.list)
  outcomes <- do.call(data.frame, params)
  return(outcomes)
}

# helper function to add intervention names to codes frame
get_alternatives_names <- function(frame, components) {
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
          string <- paste0(string, components[j])
        }
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

# get names? idk
labels_draws <- function(names) {
  results <- ifelse(
    names == "b_Intercept",
    "All Off",
    gsub(":", "", sub("^b_", "", names))
  )
  return(results)
}
