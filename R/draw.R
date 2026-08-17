#' Creates a scatterplot of expected alternative values with a frontier of
#' efficiency.
#'
#' @description
#' This function takes a value summary data frame and generates a plotly object
#' to visualize Decision Analysis for Intervention Value Efficiency. If no
#' frontier is provided, the method identifies a frontier of efficiency using
#' the outcome and cost values provided.
#'
#' @param g A daive_grid object.
#'
#' @param frontier A dataframe containing the values and costs associated with
#' interventions on the frontier of efficiency.
#' @param outs,weights Vectors of char values and numeric values containing the
#' outcome labels and weights respectively, used for generating an x axis label
#' @param static A boolean indicating whether the function should return an
#' interactive plotly object or a static ggplot object
#'
#' @export
frontier_plot <- function(g, frontier=NULL, outs=NULL, weights=NULL,
                          static=FALSE) {
  data <- g$ev_table
  make_frontier_plot(data, "value_function", "cost", frontier=frontier, outs=g$outcomes,
            weights=g$settings$weights, static=static)
}

#' Expected Value Plot
#'
#' @param g A daive_grid object
#'
#' @export
ev_plot <- function(g) {
  df <- g$ev_table

  frontier <- frontier(df, "value_function", "cost")

  draws_unscaled <- g$ev_draws[c(1:length(g$outcomes))]

  # get scaled outcomes as a list
  from <- length(g$outcomes)+1
  to <- length(g$outcomes)*2
  draws_scaled <- g$ev_draws[c(from:to)]

  # combine draws with weights
  draws_weighted <- Map(f=weight_draws, draws_scaled, g$settings$weights)
  draws_combined <- Reduce("+", draws_weighted)

  draws <- append(draws_unscaled, list(draws_combined))

  Map(f=get_ev_plot, draws, append(g$outcomes, "value_function"), MoreArgs =
        list(g=g, frontier=frontier))
}

get_ev_plot <- function(draws, outcome, g, frontier) {
  min_val  <- min(g$ev_table[, outcome], na.rm = TRUE)
  min_name <- g$ev_table$names[g$ev_table[, outcome] == min_val]

  if (length(min_name) > 1) {
    warning("Multiple rows tie for the minimum '", outcome,
            "' — using the first: ", min_name[1])
    min_name <- min_name[1]
  }

  benchmark_in_frontier <- min_name %in% frontier$names

  # avoid duplicating the row if the benchmark is already in the frontier
  if (benchmark_in_frontier) {
    names <- frontier$names
  } else {
    names <- c(min_name, frontier$names)
  }

  outcomes_frontier <- draws[, names, drop = FALSE]

  outcomes_frontier = draws[, which(colnames(draws) %in% names)]
  outcomes_frontier = outcomes_frontier[, names]
  post <- bayesplot::mcmc_intervals(outcomes_frontier, point_est="mean")
  post <- recolor_row(post, min_name)

  # only relabel as "Benchmark" if it's not already a named frontier point
  if (benchmark_in_frontier) {
    plot_labels <- setNames(names, names)
  } else {
    plot_labels <- setNames(names, names)
    plot_labels[names == min_name] <- "Benchmark"
  }

  post <- post + ggplot2::scale_y_discrete(labels = plot_labels,
                                           limits = rev(names))
}

# helper function to combine draws
weight_draws <- function(draws, weight) {
  weight*draws
}

# helper for ev_plot
recolor_row <- function(plot, row) {
  b <- ggplot2::ggplot_build(plot)

  # the discrete y values, in the order ggplot mapped them to integer positions 1, 2, 3...
  y_range <- b$layout$panel_scales_y[[1]]$range$range
  idx <- match(row, y_range)

  if (is.na(idx)) {
    stop("`row` = '", row, "' not found among plot y-axis values: ",
         paste(y_range, collapse = ", "))
  }
  target_y <- (length(y_range) + 1) - idx  # this is now numeric, matching outer_df$y etc.

  highlight_outer <- "#B2182B"
  highlight_inner <- "#67001F"
  highlight_point <- "#F4A582"

  outer_df <- b$data[[2]]
  inner_df <- b$data[[3]]
  point_df <- b$data[[4]]

  sel_outer <- outer_df[outer_df$y == target_y, ]
  sel_inner <- inner_df[inner_df$y == target_y, ]
  sel_point <- point_df[point_df$y == target_y, ]

  if (nrow(sel_outer) == 0) {
    stop("No geometry found at y = ", target_y, " for row '", row, "'.")
  }

  plot +
    ggplot2::annotate("segment",
                      x = sel_outer$x, xend = sel_outer$xend,
                      y = sel_outer$y, yend = sel_outer$yend,
                      colour = highlight_outer, linewidth = sel_outer$linewidth) +
    ggplot2::annotate("segment",
                      x = sel_inner$x, xend = sel_inner$xend,
                      y = sel_inner$y, yend = sel_inner$yend,
                      colour = highlight_inner, linewidth = sel_inner$linewidth) +
    ggplot2::annotate("point",
                      x = sel_point$x, y = sel_point$y,
                      colour = highlight_inner,
                      size = sel_point$size - .5) +
    ggplot2::annotate("point",
                      x = sel_point$x, y = sel_point$y,
                      colour = highlight_point,
                      size = sel_point$size - .6)
}

#' Creates a scatterplot of expected alternative values with a frontier of
#' efficiency.
#' @description
#' This function takes a value summary data frame and generates a plotly object
#' to visualize Decision Analysis for Intervention Value Efficiency. If no
#' frontier is provided, the method identifies a frontier of efficiency using
#' the outcome and cost values provided.
#' @param data A data frame containing a summary of expected value for each
#' alternative
#' @param outcome A char value containing the column name of the outcome
#' variable (or value function output for multiple weighted outcomes)
#' @param cost A char value containing the colname of the cost variable
#' @param frontier A data frame containing the values and costs associated with
#' interventions on the frontier of efficiency
#' @param outs,weights Vectors of char values and numeric values containing the
#' outcome labels and weights respectively, used for generating an x axis label
#' @param static A boolean indicating whether the function should return an
#' interactive plotly object or a static ggplot object
#' @return An interactive plotly object or static ggplot object (depending on
#' 'static' parameter) containing a scatterplot with all alternatives, and a
#' frontier of efficiency denoting the value efficient alternatives.
make_frontier_plot <- function(data, outcome, cost, frontier=NULL, outs=NULL,
                       weights=NULL, static=FALSE) {
  df <- data.frame(matrix(nrow = nrow(data), ncol=0))
  df$names <- data$names
  df$cost <- data[[cost]]
  df$outcome <- data[[outcome]]

  if(is.null(frontier)) {
    frontier <- frontier(df, "outcome", "cost")
  }

  df$in_frontier <- df$names %in% frontier$names

  if("extended.dominant" %in% colnames(frontier)) {
    frontier <- frontier[frontier$extended.dominant=="TRUE", ]
  }

  # Generate a label with the value function if outs and weights are included
  if(is.null(outs) || is.null(weights)) {
    xlab = outcome
  } else {
    xlab = ("Value = ")
    for(i in 1:length(outs)) {
      xlab <- paste(xlab, round(weights[i], digits=3), " * (",
                          outs[i], ")", sep = "")
      if(i < length(outs)) {
        xlab <- paste0(xlab, " + ")
      }
    }
  }

  plot.colors <- c("FALSE" = "purple", "TRUE" = "black")

  plot <- ggplot2::ggplot(df, ggplot2::aes(
    x = outcome,
    y = cost,
    color = as.factor(in_frontier),
    text = paste0(            # custom hover text for plotly
      "Name: ", names,
      "<br>Outcome: ", round(outcome, 3),
      "<br>Cost: ", round(cost, 3),
      "<br>On Frontier: ", ifelse(in_frontier, "Yes", "No")
    )
  )) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_color_manual(values = plot.colors) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 12),
      plot.margin = ggplot2::margin(10, 20, 10, 10) # more right margin for labs
    ) +
    ggplot2::labs(
      x = xlab,
      y = "Cost"
    )

  # draw the segments
  plot <- draw_frontier(frontier, plot)

  # returns static ggplot output without hoverability functionality
  if(static) {
    return(plot)
  }

  # Convert to plotly, using custom hover text
  plot <- plotly::ggplotly(plot, tooltip = "text")

  # Create plotly labels for frontier points only
  frontier.indices <- which(df$in_frontier == TRUE)

  # add hover labels
  plot <- plotly::add_annotations(
    plot,
    x          = df$outcome[frontier.indices],
    y          = df$cost[frontier.indices],
    text       = df$names[frontier.indices],
    xanchor    = "left",
    xshift     = 8,               # nudge right of point
    showarrow  = FALSE,
    font       = list(size = 12, color = "black")
  )
  plot
}

# internal function
draw_frontier <- function(df, plot) {
  for(i in 1:(nrow(df)-1)) {
    plot <- plot + ggplot2::geom_segment(x=as.numeric(df[i, 3]),
                                y=as.numeric(df[i, 2]),
                                xend=as.numeric(df[i+1, 3]),
                                yend=as.numeric(df[i+1, 2]),
                                color = "black")
  }
  plot
}
