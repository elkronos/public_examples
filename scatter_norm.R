#' Plot rescaled x and y variables
#'
#' This function creates a scatter plot of two rescaled variables, with options for labeling correlation coefficients,
#' annotating quadrants, and saving the output. It supports both static and interactive (plotly) plots.
#'
#' @param data A data frame containing the x and y variables.
#' @param x The x variable to be plotted.
#' @param y The y variable to be plotted.
#' @param r_label Logical. If TRUE, the correlation coefficient will be labeled on the plot. Default is TRUE.
#' @param save_path Character. The file path where the plot will be saved. Default is NULL, which means the plot will not be saved.
#' @param interactive Logical. If TRUE, the plot will be rendered as an interactive plotly plot. Default is FALSE.
#' @param annotate_quadrants Logical. If TRUE, the plot will be annotated with quadrant percentages. Default is FALSE.
#' @return A ggplot object, either as a static plot or interactive plotly plot, depending on the interactive parameter.
#' 
#' @examples
#' # Basic example:
#' scatter_norm(mtcars, mpg, disp)
#'
#' # With quadrant annotations:
#' scatter_norm(mtcars, mpg, disp, annotate_quadrants = TRUE)
#'
#' # Without the correlation coefficient label:
#' scatter_norm(mtcars, mpg, disp, r_label = FALSE)
#'
#' # Save the plot to a file:
#' scatter_norm(mtcars, mpg, disp, save_path = "rescaled_plot.png")
#'
#' # Render the plot as an interactive plotly plot:
#' scatter_norm(mtcars, mpg, disp, interactive = TRUE)
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export

# Load packages
library(ggplot2)
library(plotly)

# Save function
scatter_norm <- function(data, x, y, r_label = TRUE, save_path = NULL, interactive = FALSE, annotate_quadrants = FALSE) {
  
  # Rescale the variables so they have similar scales centered at 0
  x_rescaled <- (data[[as.character(substitute(x))]] - mean(data[[as.character(substitute(x))]])) / sd(data[[as.character(substitute(x))]])
  y_rescaled <- (data[[as.character(substitute(y))]] - mean(data[[as.character(substitute(y))]])) / sd(data[[as.character(substitute(y))]])
  
  # Create the scatter plot with a loess smooth
  p <- ggplot(data.frame(x_rescaled, y_rescaled), aes(x_rescaled, y_rescaled))
  p <- p + geom_point() +
    geom_smooth(method = "loess")
  
  if (r_label) {
    r <- round(cor(data[[as.character(substitute(x))]], data[[as.character(substitute(y))]]), 2)
    p <- p + annotate("text", x = min(x_rescaled), y = max(y_rescaled), label = paste("r =", r),
                      hjust = 0, vjust = 1, fontface = "italic", color = "darkblue", alpha = 0.6)
  }
  
  if (annotate_quadrants && length(x_rescaled) >= 4) {
    quadrant_count <- table(cut(x_rescaled, breaks = c(-Inf, 0, Inf), include.lowest = TRUE),
                            cut(y_rescaled, breaks = c(-Inf, 0, Inf), include.lowest = TRUE))
    quadrant_pct <- prop.table(quadrant_count) * 100
    
    quadrant_labels <- c("Q1: ", "Q2: ", "Q3: ", "Q4: ")
    quadrant_labels <- paste0(quadrant_labels, round(as.numeric(c(quadrant_pct[2], quadrant_pct[4], quadrant_pct[1], quadrant_pct[3])), 1), "%")
    
    x_annotate <- quantile(x_rescaled, c(0.25, 0.75, 0.25, 0.75))
    y_annotate <- quantile(y_rescaled, c(0.75, 0.75, 0.25, 0.25))
    p <- p + annotate("text", x = x_annotate, y = y_annotate, label = quadrant_labels,
                      hjust = 0, vjust = 1, fontface = "italic", color = "darkblue", alpha = 0.6)
  }
  
  p <- p + geom_hline(yintercept = 0, linewidth = 1) +
    geom_vline(xintercept = 0, linewidth = 1) +
    xlab(paste("Rescaled", deparse(substitute(x)))) +
    ylab(paste("Rescaled", deparse(substitute(y)))) +
    coord_equal()
  
  if (!is.null(save_path)) {
    ggsave(save_path, p)
  }
  
  if (interactive) {
    
    ggplotly(p)
  } else {
    p
  }
}