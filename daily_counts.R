library(data.table)
library(ggplot2)
library(scales)

#' Calculate Daily Counts
#'
#' This function calculates the daily counts based on start and end dates in a data.table.
#'
#' @param data A data.table object containing the data.
#' @param start_col A character string specifying the column name for the start dates.
#' @param end_col A character string specifying the column name for the end dates.
#' @param group_col A character string specifying the column name for grouping the data (optional).
#'
#' @return A data.table object with the daily counts.
#'
#' @import data.table
#'
#' @examples
#' # Create a data.table
#' data <- data.table(
#'   group = c("A", "A", "B", "B"),
#'   start = as.Date(c("1999-01-02", "1999-01-03", "1999-01-03", "1999-01-06")),
#'   end = as.Date(c("1999-01-05", "1999-01-06", "1999-01-09", "1999-01-06"))
#' )
#'
#' # Use the function
#' result <- daily_counts(data, "start", "end")
#' print(result)
#'
#' # Use the function with grouping
#' result_group <- daily_counts(data, "start", "end", "group")
#' print(result_group)
daily_counts <- function(data, start_col, end_col, group_col=NULL) {
  # Ensure input is data.table
  if (!is.data.table(data)) {
    stop("Input data must be a data.table.")
  }
  
  # Check if provided columns exist
  if (!(all(c(start_col, end_col) %in% colnames(data)))) {
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
  expanded_data <- data[, .(Date = unlist(lapply(1:.N, function(i) seq(start[i], end[i], by = "day")))), by = group_col]
  
  # Count the number of times each date appears in each group
  result <- expanded_data[all_dates, on = names(all_dates), .N, by = .EACHI]
  
  # Replace NA with 0
  result[is.na(N), N := 0]
  
  result
}

#' Plot Daily Counts
#'
#' This function plots daily counts from a given dataset. It can either plot overall daily counts or grouped daily counts based on a specified group column.
#'
#' @param result The dataset containing the daily counts.
#' @param group_col The column name indicating the groups. Default is \code{NULL}.
#'
#' @return A ggplot object representing the plot of daily counts.
#'
#' @import ggplot2
#' @importFrom scales hue_pal
#' @importFrom rlang sym
#' @importFrom dplyr get
#'
#' @examples
#' # Example 1: Plot overall daily counts
#' plot_daily_counts(result)
#'
#' # Example 2: Plot grouped daily counts by a specified column
#' plot_daily_counts(result, group_col = "Group")
plot_daily_counts <- function(result, group_col = NULL) {
  # Check if group column is provided
  if (!is.null(group_col)) {
    groups <- unique(result[[group_col]])
    num_groups <- length(groups)
    
    # Generate color palette
    color_palette <- hue_pal()(num_groups)
    
    # Plot grouped daily counts with color by groups
    ggplot(result, aes(Date, N, group = !!rlang::sym(group_col), color = factor(get(group_col)))) +
      geom_line(linetype = "solid", size = 1.5) +
      geom_point(size = 3, shape = 21, fill = "white") +
      labs(x = "Date", y = "Count", title = "Daily Counts by Group") +
      scale_color_manual(values = color_palette) +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.title = element_blank(),
            legend.text = element_text(size = 12))
  } else {
    # Plot overall daily counts
    ggplot(result, aes(Date, N)) +
      geom_line(color = "#0072B2FF", linetype = "solid", size = 1.5) +
      geom_point(color = "#0072B2FF", size = 3, shape = 21, fill = "white") +
      labs(x = "Date", y = "Count", title = "Overall Daily Counts") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
  }
}