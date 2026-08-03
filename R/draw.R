#' @export
frontier_plot <- function(g, frontier=NULL, outs=NULL, weights=NULL,
                          static=FALSE) {
  data <- g$ev_table
  make_frontier_plot(data, "value_function", "cost", frontier=frontier, outs=g$outcomes,
            weights=g$settings$weights, static=static)
}

#' @export
ev_plot <- function(g) {
  df <- g$ev_table

  frontier <- frontier(df, "value_function", "cost")

  draws_unscaled <- g$ev_draws[c(1:length(g$outcomes))]

  Map(f=get_ev_plot, draws_unscaled, g$outcomes, MoreArgs = list(g=g, frontier=frontier))
}

get_ev_plot <- function(draws, outcome, g, frontier) {
  min <- min(g$ev_table[, outcome])
  min <- g$ev_table$names[g$ev_table[, outcome] == min]

  names <- append(min, frontier$names)
  #return(names)

  outcomes_frontier = draws[, which(colnames(draws) %in% names)]
  outcomes_frontier = outcomes_frontier[, names]
  post <- bayesplot::mcmc_intervals(outcomes_frontier, point_est="mean") #+
  #  scale_x_continuous(limits=c(0,1))
  post <- recolor_min(post)
  plot_labels = append("Benchmark", frontier$names)
  plot_labels <- setNames(plot_labels, names)
  post <- post + ggplot2::scale_y_discrete(labels = plot_labels,
                                           limits = rev(names))
  post
}

# helper for ev_plots
recolor_min <- function(plot) {
  b <- ggplot2::ggplot_build(plot)

  highlight_outer <- "#B2182B"   # thin line color
  highlight_inner <- "#67001F"   # thick line color
  highlight_point <- "#F4A582"   # point color

  outer_df <- b$data[[2]]
  inner_df <- b$data[[3]]
  point_df <- b$data[[4]]

  top_y <- max(outer_df$y)

  top_outer <- outer_df[outer_df$y == top_y, ]
  top_inner <- inner_df[inner_df$y == top_y, ]
  top_point <- point_df[point_df$y == top_y, ]

  output <- plot +
    ggplot2::annotate("segment",
             x = top_outer$x, xend = top_outer$xend,
             y = top_outer$y, yend = top_outer$yend,
             colour = highlight_outer, linewidth = top_outer$linewidth) +
    ggplot2::annotate("segment",
             x = top_inner$x, xend = top_inner$xend,
             y = top_inner$y, yend = top_inner$yend,
             colour = highlight_inner, linewidth = top_inner$linewidth) +
    ggplot2::annotate("point",
             x = top_point$x, y = top_point$y,
             colour = highlight_inner,                 # this becomes the "border"
             size = top_point$size - .5) +    # slightly bigger, sits behind
    ggplot2::annotate("point",
             x = top_point$x, y = top_point$y,
             colour = highlight_point,
             size = top_point$size - .6)

  return(output)
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
