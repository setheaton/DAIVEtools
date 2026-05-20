# Takes a frontier and a plot as a inputs and draws the frontier over it
#' @export
DAIVE_plot <- function(data, outcome, cost, frontier=NULL) {
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

  plot.colors <- c("FALSE" = "darkred", "TRUE" = "black")

  plot <- ggplot2::ggplot(df, aes(outcome, cost,
                                  color=as.factor(in.frontier))) +
    ggplot2::geom_point() +
    ggplot2:: scale_color_manual(values = plot.colors) +
    ggplot2::geom_text(
      data = df[df$in.frontier == TRUE,], aes(label = names), color = "black",
      nudge_x = 0.002) +
    ggplot2::theme(legend.position="none")
  plot <- draw_frontier(frontier, plot)
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
