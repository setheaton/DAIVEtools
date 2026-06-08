#' Generates table of expected values (STILL IN DEVELOPMENT)
#' @export
values_table <- function(draws, outs, components, codes) {
  # take a vector of bayesian models as a parameter
  # take a vector of component column names

  # get number of components
  k <- length(components)

  # set up outcomes frame
  outcomes <- get_outcomes_asframe(draws[[1]], codes)
  colnames(outcomes) <- append(components, outs[1])

  if (length(draws) > 1) {
    for (i in 2:length(draws)) {
      outcomes$tmp <- get_outcomes_asframe(draws[[i]], codes)$out
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
      subset <- match_df(outcomes, value.summary[i,1:length(components)])
      value.summary[i, outs[j]] <- mean(subset[[outs[j]]])
      scale <- paste0(outs[j], ".scale")
      value.summary[i, scale] <- mean(subset[[scale]])
    }
  }

  return(value.summary)
}

# helper function to get outcomes as a dataframe from draws
get_outcomes_asframe <- function(draws, codes) {

  # For a given draw index i, extract draw i from the posterior as a matrix and
  # left-multiply by codes to produce the outcome matrix for that draw
  out.list <- lapply(unique(draws$.draw), function(i) {
    draw_matrix <- draws |> filter(.draw == i) |> select(4:length(draws)) |> as.matrix()
    as.matrix(codes) %*% t(draw_matrix)
  })

  # add outcomes data to a frame with effects codes and return
  # To-DO: MAKE THIS RESPONSIVE TO VARIOUS NUMBERS OF COMPONENTS
  outcomes = data.frame(A = rep(c(codes$A), 4000),
                        B = rep(c(codes$B), 4000),
                        C = rep(c(codes$C), 4000),
                        D = rep(c(codes$D), 4000),
                        #E = rep(c(codes$E), 4000),
                        out = unlist(out.list))
  return(outcomes)
}

# helper function to add intervention names to codes frame for k = 5
get_alternatives_names <- function(value.summary, components) {
  # get number of components
  k <- length(components)

  # initialize names vector
  names.vec <- c()
  for (i in 1:2^k) {
    string <- ""
    if (sum(value.summary[i, 1:k]) == -1*k) {
      string = "All Off"
    } else {
      for (j in 1:k){
        if(value.summary[i, j] == 1) {
          string <- paste0(string, components[j])
        }
      }
    }
    names.vec <- append(names.vec, string)
  }
  value.summary$names <- names.vec
  return(value.summary)
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
