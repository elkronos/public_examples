#' Time Series Bar Plot with Moving Line
#'
#' This function creates a time series bar plot with a moving line based on aggregated data. It allows for grouping the data by different time intervals such as day, week, month, quarter, or year, and calculates the aggregated values for the bars and the moving line.
#'
#' @param data A data frame containing the time series data.
#' @param date_col The name of the column in the \code{data} frame that contains the dates.
#' @param y_col The name of the column in the \code{data} frame that contains the values.
#' @param group_by The time interval for grouping the data. Possible values are "day", "month", "quarter", or "year". The default is "day".
#' @param window_size The size of the moving window used to calculate the moving line. The default is 7.
#' @param bar_stat The statistic to be used for calculating the values displayed in the bars. Possible values are "sum", "mean", or "median". The default is "sum".
#' @param line_stat The statistic to be used for calculating the values displayed in the moving line. Possible values are "mean", "median", "max", or "min". The default is "mean".
#'
#' @return A \code{ggplot} object representing the time series bar plot with the moving line.
#'
#' @importFrom ggplot2 ggplot geom_bar geom_line scale_x_date theme labs
#' @importFrom dplyr mutate select group_by summarize arrange
#' @importFrom scales date_format date_breaks
#' @importFrom zoo rollapply
#' @importFrom lubridate as.Date floor_date ymd
#'
#' @examples
#' set.seed(123)
#' data <- data.frame(date = seq(as.Date("2010-01-01"), as.Date("2020-12-31"), by = "day"),
#'                    value = rnorm(4018, 10, 2))
#'
#' # No aggregation example
#' bar_time(data, date_col = "date", y_col = "value", window_size = 7, bar_stat = "sum", line_stat = "mean") -> plot_daily
#' print(plot_daily)
#'
#' # Monthly aggregation example
#' bar_time(data, date_col = "date", y_col = "value", group_by = "month", window_size = 3, bar_stat = "median", line_stat = "max") -> plot_monthly
#' print(plot_monthly)
#'
#' # Quarterly aggregation example
#' bar_time(data, date_col = "date", y_col = "value", group_by = "quarter", window_size = 2, bar_stat = "sum", line_stat = "min") -> plot_quarterly
#' print(plot_quarterly)
#'
#' # Yearly aggregation example
#' bar_time(data, date_col = "date", y_col = "value", group_by = "year", window_size = 2, bar_stat = "mean", line_stat = "mean") -> plot_yearly
#' print(plot_yearly)

library(ggplot2)
library(dplyr)
library(scales)
library(zoo)
library(lubridate)

bar_time <- function(data, date_col, y_col, group_by = "day", window_size = 7, bar_stat = "sum", line_stat = "mean") {
  
  if (!is.character(date_col) || !is.character(y_col) || !is.character(group_by)) {
    stop("The date_col, y_col, and group_by arguments must be character.")
  }
  
  if (!is.numeric(window_size)) {
    stop("The window_size argument must be numeric.")
  }
  
  if (!bar_stat %in% c("sum", "mean", "median")) {
    stop("The bar_stat argument must be one of 'sum', 'mean', or 'median'.")
  }
  
  if (!line_stat %in% c("mean", "median", "max", "min")) {
    stop("The line_stat argument must be one of 'mean', 'median', 'max', or 'min'.")
  }
  
  data <- data %>%
    mutate(date = as.Date(!!sym(date_col))) %>%
    mutate(y = as.numeric(!!sym(y_col))) %>%
    select(date, y)
  
  if (group_by == "month") {
    data <- data %>%
      mutate(date = as.Date(format(date, "%Y-%m-01")))
  } else if (group_by == "year") {
    data <- data %>%
      mutate(date = as.Date(format(date, "%Y-01-01")))
  } else if (group_by == "quarter") {
    data <- data %>%
      mutate(date = ymd(date)) %>%
      mutate(date = floor_date(date, unit = "quarter"))  # Adjusting date to the start of the quarter
  }
  
  data_summary <- data %>%
    group_by(date) %>%
    summarize(y_sum = sum(y, na.rm = TRUE),
              y_avg = mean(y, na.rm = TRUE),
              y_median = median(y, na.rm = TRUE))
  
  data_summary <- data_summary %>%
    arrange(date)
  
  moving_stat <- switch(
    line_stat,
    mean = rollapply(data_summary$y_avg, width = window_size, FUN = mean, align = "right", fill = NA),
    median = rollapply(data_summary$y_median, width = window_size, FUN = median, align = "right", fill = NA),
    max = rollapply(data_summary$y_avg, width = window_size, FUN = max, align = "right", fill = NA),
    min = rollapply(data_summary$y_sum, width = window_size, FUN = min, align = "right", fill = NA)
  )
  
  if (bar_stat == "sum") {
    data_summary <- data_summary %>%
      mutate(bar_value = y_sum)
  } else if (bar_stat == "mean") {
    data_summary <- data_summary %>%
      mutate(bar_value = y_avg)
  } else if (bar_stat == "median") {
    data_summary <- data_summary %>%
      mutate(bar_value = y_median)
  }
  
  date_format_string <- ifelse(group_by == "day", "%Y-%m-%d",
                               ifelse(group_by == "month", "%Y-%m",
                                      ifelse(group_by == "year", "%Y",
                                             ifelse(group_by == "quarter", "Q%q, %Y", ""))))
  
  date_break_interval <- ifelse(group_by == "day", "1 month",
                                ifelse(group_by == "month", "6 months",
                                       ifelse(group_by == "year", "1 year",
                                              ifelse(group_by == "quarter", "6 months", ""))))
  
  plot <- ggplot(data_summary, aes(x = date, y = bar_value)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
    geom_line(aes(y = moving_stat), color = "red", na.rm = TRUE, linewidth = 1, position = "identity") +
    scale_x_date(labels = date_format(date_format_string), breaks = date_breaks(date_break_interval)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date", y = bar_stat, title = paste("Time Series Bar Plot of", bar_stat, "with rolling", line_stat))
  
  return(plot)
}