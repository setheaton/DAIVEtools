#' Generates table of expected values (STILL IN DEVELOPMENT)
#' @export
values_table <- function(draws1, draws2, draws3, codes) {
  # take a vector of bayesian models as a parameter
  # take a vector of component column names

  outcomes <- get_outcomes_asframe(draws1, codes)
  colnames(outcomes) <- c("A", "B", "C", "D", "E", "out1")
  outcomes$out2 <- get_outcomes_asframe(draws2, codes)$out
  outcomes$out3 <- get_outcomes_asframe(draws3, codes)$out

  outcomes <- scale_outcome(outcomes, "out1", "out1.scale")
  outcomes <- scale_outcome(outcomes, "out2", "out2.scale")
  outcomes <- scale_outcome(outcomes, "out3", "out3.scale")

  value.summary <- codes[,2:6]
  value.summary <- get_alternatives_names(value.summary)

  # iterate through alternatives
  for (i in 1:32) {
    # get subset of outcomes for that alternative
    subset = match_df(outcomes, value.summary[i,1:5])
    value.summary$out1[i] = mean(subset$out1)
    value.summary$out2[i] = mean(subset$out2)
    value.summary$out3[i] = mean(subset$out3)
    value.summary$out1.scale[i] = mean(subset$out1.scale)
    value.summary$out2.scale[i] = mean(subset$out2.scale)
    value.summary$out3.scale[i] = mean(subset$out3.scale)
  }

  return(value.summary)
}

# helper function to get outcomes as a dataframe from draws
get_outcomes_asframe <- function(draws, codes) {

  # For a given draw index i, extract draw i from the posterior as a matrix and
  # left-multiply by codes to produce the outcome matrix for that draw
  out.list <- lapply(unique(draws$.draw), function(i) {
    draw_matrix <- draws |> filter(.draw == i) |> select(4:35) |> as.matrix()
    as.matrix(codes) %*% t(draw_matrix)
  })

  # add outcomes data to a frame with effects codes and return
  outcomes = data.frame(A = rep(c(codes$A), 4000),
                        B = rep(c(codes$B), 4000),
                        C = rep(c(codes$C), 4000),
                        D = rep(c(codes$D), 4000),
                        E = rep(c(codes$E), 4000),
                        out = unlist(out.list))
  return(outcomes)
}

# helper function to add intervention names to codes frame for k = 5
get_alternatives_names <- function(value.summary) {
  names.vec <- c()
  for (i in 1:32) {
    string <- ""
    if (sum(value.summary[i, 1:5]) == -5) {
      string = "All Off"
    } else {
      if (value.summary[i, 1] == 1) {
        string <- paste0(string, "A")
      }
      if (value.summary[i, 2] == 1) {
        string <- paste0(string, "B")
      }
      if (value.summary[i, 3] == 1) {
        string <- paste0(string, "C")
      }
      if (value.summary[i, 4] == 1) {
        string <- paste0(string, "D")
      }
      if (value.summary[i, 5] == 1) {
        string <- paste0(string, "E")
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
