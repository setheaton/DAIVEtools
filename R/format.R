#' Generates table of expected values (STILL IN DEVELOPMENT)
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

#' Format tidybayes::spread_draws output for estimating alternative performance
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
  # To-DO: MAKE THIS RESPONSIVE TO VARIOUS NUMBERS OF COMPONENTS
  # temporary solution
  if(k == 4) {
  outcomes = data.frame(A = rep(c(codes$A), 4000),
                        B = rep(c(codes$B), 4000),
                        C = rep(c(codes$C), 4000),
                        D = rep(c(codes$D), 4000),
                        out = unlist(out.list))
  } else {
    if(k == 4) {
      outcomes = data.frame(A = rep(c(codes$A), 4000),
                            B = rep(c(codes$B), 4000),
                            C = rep(c(codes$C), 4000),
                            D = rep(c(codes$D), 4000),
                            E = rep(c(codes$E), 4000),
                            out = unlist(out.list))
    }
  }
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
