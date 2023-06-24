library(data.table)
library(ggplot2)
#' Daily Summary
#'
#' This function calculates summary statistics for a given data set based on
#' specified date ranges.
#'
#' @param data A data.table object containing the data set.
#' @param start_col A character string specifying the name of the column that
#'   contains the start dates of the ranges.
#' @param end_col A character string specifying the name of the column that
#'   contains the end dates of the ranges.
#' @param value_col A character string specifying the name of the column that
#'   contains the values to be summarized.
#' @param group_col An optional character string specifying the name of the column
#'   that defines groups for summarization. If NULL, no grouping will be performed.
#' @param summary_func A character string specifying the summary function to be
#'   used. Allowed values are "sum" (default), "mean", and "median".
#'
#' @return A data.table object with columns "Date" and "N", where "Date" represents
#'   all dates within the specified ranges, and "N" represents the calculated
#'   summary statistic.
#'
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot2
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
#'
#' # Create a data.table
#' data <- data.table(
#'   group = c("A", "A", "A", "A", "B", "B", "B", "B"),
#'   start = as.Date(c("1999-01-02", "1999-01-02", "1999-01-03", "1999-01-03", "1999-01-06", "1999-01-06", "1999-01-07", "1999-01-07")),
#'   end = as.Date(c("1999-01-05", "1999-01-05", "1999-01-06", "1999-01-06", "1999-01-09", "1999-01-09", "1999-01-10", "1999-01-10")),
#'   value = c(5, 4, 8, 2, 4, 1, 5, 3)
#' )
#'
#' # Use the function for sum
#' result_sum <- daily_summary(data, "start", "end", "value", "group", "sum")
#' print(result_sum)
#'
#' # Use the function for mean
#' result_mean <- daily_summary(data, "start", "end", "value", "group", "mean")
#' print(result_mean)
#'
#' # Use the function for median
#' result_median <- daily_summary(data, "start", "end", "value", "group", "median")
#' print(result_median)
#'
#' # Combine the mean and median results
#' result <- merge(result_mean, result_median, by = c("group", "Date"), suffixes = c("_mean", "_median"))
#'
#' # Convert Date column to Date format
#' result$Date <- as.Date(result$Date)
#'
#' # Create the plot
#' plot <- ggplot(result, aes(x = Date)) +
#'   geom_line(aes(y = N_mean, color = group, linetype = "Mean"), size = 1) +
#'   geom_line(aes(y = N_median, color = group, linetype = "Median"), size = 1) +
#'   scale_color_manual(values = c("A" = "blue", "B" = "red")) +
#'   scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dashed")) +
#'   labs(x = "Date", y = "Summary", color = "Group", linetype = "Statistic") +
#'   theme_minimal() +
#'   theme(
#'     legend.position = "top",
#'     panel.grid.major = element_blank(),
#'     panel.grid.minor = element_blank(),
#'     axis.line = element_line(color = "black"),
#'     axis.text = element_text(color = "black"),
#'     axis.title = element_text(color = "black"),
#'     plot.title = element_text(color = "black", size = 14, face = "bold")
#'   )
#'
#' # Display the plot
#' print(plot)
daily_summary <- function(data, start_col, end_col, value_col, group_col=NULL, summary_func="sum") {
  # Ensure input is data.table
  if (!is.data.table(data)) {
    stop("Input data must be a data.table.")
  }
  
  # Check if provided columns exist
  if (!(all(c(start_col, end_col, value_col) %in% colnames(data)))) {
    stop("One or more specified columns do not exist in the input data.")
  }
  
  # Convert to date format if necessary
  if (!inherits(data[[start_col]], "Date")) data[[start_col]] <- as.Date(data[[start_col]])
  if (!inherits(data[[end_col]], "Date")) data[[end_col]] <- as.Date(data[[end_col]])
  
  # Check if start dates are always before or equal to end dates
  if (any(data[[start_col]] > data[[end_col]])) {
    stop("Start dates must be less than or equal to end dates.")
  }
  
  # Create a list of all dates
  all_dates <- seq(min(data[[start_col]]), max(data[[end_col]]), by = "day")
  
  # Generate unique combinations of group and all dates if group is specified
  if (!is.null(group_col)) {
    all_dates <- CJ(unique(data[[group_col]]), all_dates)
    setnames(all_dates, names(all_dates), c(group_col, "Date"))
  } else {
    all_dates <- data.table(Date = all_dates)
  }
  
  # Expand the input data to include every date in each date range
  expanded_data <- data[, .(Date = unlist(lapply(1:.N, function(i) seq(start[i], end[i], by = "day"))),
                            Value = rep(value, times = pmax(0, end - start + 1))), by = group_col]
  
  # Use the requested summary function for each date in each group
  if (summary_func == "sum") {
    result <- expanded_data[all_dates, on = names(all_dates), .(N = sum(Value, na.rm=TRUE)), by = .EACHI]
  } else if (summary_func == "mean") {
    result <- expanded_data[all_dates, on = names(all_dates), .(N = mean(Value, na.rm=TRUE)), by = .EACHI]
  } else if (summary_func == "median") {
    result <- expanded_data[all_dates, on = names(all_dates), .(N = median(Value, na.rm=TRUE)), by = .EACHI]
  } else {
    stop("Invalid summary function. Choose 'sum', 'mean', or 'median'.")
  }
  
  # Replace NA with 0
  result[is.na(N), N := 0]
  
  result
}