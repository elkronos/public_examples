#' Plot Time Series with Optional Filled Area Between Two Series
#'
#' This function creates a line plot for time series data. If two series are provided and `fill_between` is TRUE,
#' it fills the area between the two series with the respective colors.
#'
#' @importFrom ggplot2 ggplot aes_string geom_segment geom_line labs theme_light theme element_blank element_line
#'
#' @param data A data frame containing the time series data.
#' @param time_col A string specifying the name of the column in `data` that contains the time values.
#' @param series_cols A character vector specifying the names of the columns in `data` that contain the series data. If `fill_between` is TRUE, the length of this vector must be exactly 2.
#' @param colors A character vector of colors for the lines. The length of this vector should match the length of `series_cols`.
#' @param fill_between A logical value. If TRUE, the area between the two series will be filled. Default is FALSE.
#' @param title A string to be used as the plot title. Default is NULL.
#' @param xlab A string to be used as the x-axis label. Default is NULL.
#' @param ylab A string to be used as the y-axis label. Default is NULL.
#' @param alpha_val A numeric value between 0 and 1 for setting the alpha (transparency) of the filled area between the series. Default is 0.5.
#' @param line_width A numeric value specifying the width of the lines. Default is 2.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(
#'   time = seq.Date(from = as.Date("2021-01-01"), by = "month", length.out = 24),
#'   series1 = c(rnorm(8, 10, 2), rnorm(8, 15, 2), rnorm(8, 10, 2)),
#'   series2 = c(rnorm(8, 15, 2), rnorm(8, 10, 2), rnorm(8, 15, 2))
#' )
#'
#' plot_timeseries_fill(df, "time", c("series1", "series2"), fill_between = TRUE, colors = c("blue", "red"), title = "Crossover Example", xlab = "Date", ylab = "Value")
#' }
#'
#' @export
plot_timeseries_fill <- function(data, time_col, series_cols, colors = NULL, fill_between = FALSE,
                                 title = NULL, xlab = NULL, ylab = NULL, alpha_val = 0.5, line_width = 2) {
  
  if (fill_between && length(series_cols) != 2) {
    stop("fill_between option requires exactly two series.")
  }
  
  # Define which line is on top for coloring the bars
  data$top_color = ifelse(data[[series_cols[1]]] <= data[[series_cols[2]]], colors[1], colors[2])
  
  # Initialize the plot
  p <- ggplot(data, aes_string(x = time_col))
  
  # Add bars between the lines if fill_between is TRUE
  if(fill_between) {
    p <- p + geom_segment(aes(xend = !!sym(time_col), y = !!sym(series_cols[1]), yend = !!sym(series_cols[2]), color = top_color), alpha = alpha_val, size = line_width/2, lineend = 'round')
  }
  
  # Add lines for each series
  for (i in seq_along(series_cols)) {
    p <- p + geom_line(aes_string(y = series_cols[i]), color = colors[i], size = line_width)
  }
  
  p <- p + labs(title = title, x = xlab, y = ylab) + theme_light() +
    theme(panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(color = "black"))
  
  return(p)
}