#' Generates scatterplot of expected values with frontier of efficiency
#' @export
DAIVE_plot <- function(data, outcome, cost, frontier=NULL, outs=NULL,
                       weights=NULL) {
  df <- data.frame(matrix(nrow = nrow(data), ncol=0))
  df$names <- data$names
  df$cost <- data[[cost]]
  df$outcome <- data[[outcome]]

  if(is.null(frontier)) {
    frontier <- frontier(df, "outcome", "cost")
  }

  df$in.frontier <- df$names %in% frontier$names

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
    color = as.factor(in.frontier),
    text = paste0(            # custom hover text for plotly
      "Name: ", names,
      "<br>Outcome: ", round(outcome, 3),
      "<br>Cost: ", round(cost, 3),
      "<br>On Frontier: ", ifelse(in.frontier, "Yes", "No")
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

  # Convert to plotly, using custom hover text
  plot <- plotly::ggplotly(plot, tooltip = "text")

  # Create plotly labels for frontier points only
  frontier.indices <- which(df$in.frontier == TRUE)

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
  return(plot)
}

draw_frontier <- function(df, plot) {
  for(i in 1:(nrow(df)-1)) {
    plot <- plot + ggplot2::geom_segment(x=as.numeric(df[i, 3]),
                                y=as.numeric(df[i, 2]),
                                xend=as.numeric(df[i+1, 3]),
                                yend=as.numeric(df[i+1, 2]),
                                color = "black")
  }
  return(plot)
}
