#' Calculates frontier of efficiency.
#' @export
frontier <- function(data, val, cost, extended = TRUE) {
  # set up a tmp df with the names, costs, and value for the dataframe
  df <- data.frame(matrix(nrow = nrow(data), ncol=0))
  df$names <- data$names
  df$cost <- data[[cost]]
  df$val <- data[[val]]

  # start recursive function to find set of candidates for efficiency frontier
  candidate <- data.frame(matrix(nrow=0,ncol=ncol(df)))
  subset <- get_candidates(df, candidate)

  # initialize frontier
  frontier <- data.frame(matrix(nrow=0,ncol=ncol(df)))

  # select minimum slope until most effective alternative is reached
  subset <- get_slopes_cost(subset)
  while(nrow(subset) > 1) {
    # add the NaN row (returned by min) to the frontier and remove it
    min <- min(subset$slopes)
    idx <- match(min, subset$slopes)
    frontier <- rbind(frontier, subset[idx, c(1:3)])
    subset <- subset[-idx,]
    #view(subset)

    # find the actual minimum value and get index
    min <- min(subset$slopes)
    #print(min)
    idx <- match(min, subset$slopes)
    #print(idx)

    # remove alternatives with higher slopes that are less effective than min
    subset <- subset(subset, val >= subset[idx, "val"])

    # calculate new slopes
    subset <- get_slopes_slope(subset)
  }
  frontier <- rbind(frontier, subset[, c(1:3)])
  colnames(frontier) <- c("names", cost, val)

  if (!extended) {
    candidate <- data.frame(matrix(nrow=0,ncol=ncol(df)))
    band <- get_candidates(df, candidate)
    band$extended.dominant <- band$names %in% frontier$names
    frontier <- band
  }
  return(frontier)
}

# identifies the set of candidates that are more effective than all other
# interventions with lesser costs
get_candidates <- function(tmp, candidate) {
  # if one row left, terminate
  if (nrow(tmp) <= 1){
    candidate <- rbind(candidate, tmp)
    return(candidate)
  }

  # otherwise, find most effective intervention left, add it to candidate set
  # and run again on subset of all interventions that cost less than that
  # intervention
  max <- max(tmp$val)
  idx <- match(max, tmp$val)
  subset <- subset(tmp, cost < tmp[idx, "cost"])
  candidate <- rbind(candidate, tmp[idx, ])
  tmp <- subset
  return(get_candidates(tmp, candidate))
}

# get the initial slope calculations based on least cost
get_slopes_cost <- function(tmp) {
  min <- min(tmp$cost)
  idx <- match(min, tmp$cost)
  tmp$slopes <- get_slope(as.numeric(tmp[idx, "cost"]),
                          as.numeric(tmp[idx, "val"]), tmp$cost, tmp$val)
  return(tmp)
}

# get slope calculations based on last addition to frontier
get_slopes_slope <- function(tmp) {
  min <- min(tmp$slope)
  idx <- match(min, tmp$slope)
  tmp$slopes <- get_slope(as.numeric(tmp[idx, "cost"]),
                          as.numeric(tmp[idx, "val"]), tmp$cost, tmp$val)
  return(tmp)
}

# helper func to calculate slope
get_slope <- function(rise1, run1, rise2, run2) {
  rise <- (rise2 - rise1)
  run <- (run2 - run1)
  return(rise / run)
}
