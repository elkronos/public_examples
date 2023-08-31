library(data.table)
library(ggplot2)
library(scales)
library(rlang)

#' Calculate Daily Counts
#'
#' This function calculates the daily counts of records spanning 
#' across multiple days based on start and end dates provided in a data.table.
#' If a grouping column is provided, the daily counts are split based on the groups.
#'
#' @param data A data.table object containing the data.
#' @param start_col A character string specifying the column name for the start dates.
#' @param end_col A character string specifying the column name for the end dates.
#' @param group_col A character string specifying the column name for grouping the data (optional).
#'
#' @return A data.table object with the daily counts. Each record will have 
#' a date and a count, and if grouped, a group identifier.
#'
#' @importFrom data.table as.data.table setkey foverlaps rbindlist setnames
#'
#' @details 
#' The function works by creating a sequence of dates between the minimum and 
#' maximum dates provided in the input data. It then counts the occurrences 
#' of these dates between the start and end dates.
#' If a group_col is provided, the function will segment the data by the unique 
#' values in this column and calculate daily counts for each segment separately.
#'
#' @examples
#' \dontrun{
#' # Create a data.table
#' data <- data.table(
#'   start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05")),
#'   end = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08"))
#' )
#'
#' # Use the function without grouping
#' result <- daily_counts(data, "start", "end")
#' print(result)
#'
#' # Create another data.table with grouping
#' data_grouped <- data.table(
#'   start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05", "2023-01-03")),
#'   end = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08", "2023-01-04")),
#'   group = c("A", "A", "B", "C")
#' )
#'
#' # Use the function with grouping
#' result_grouped <- daily_counts(data_grouped, "start", "end", "group")
#' print(result_grouped)
#' }
daily_counts <- function(data, start_col, end_col, group_col=NULL) {
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }
  
  # Temporary rename for internal consistency
  data <- data[, temp_start := get(start_col)]
  data <- data[, temp_end := get(end_col)]
  
  if (!inherits(data$temp_start, "Date")) data$temp_start <- as.Date(data$temp_start)
  if (!inherits(data$temp_end, "Date")) data$temp_end <- as.Date(data$temp_end)
  
  if (any(data$temp_start > data$temp_end)) {
    stop("Start dates must be less than or equal to end dates.")
  }
  
  results_list <- list()
  
  if (is.null(group_col)) {
    all_dates <- data.table(temp_start = seq(min(data$temp_start), max(data$temp_end), by = "day"),
                            temp_end = seq(min(data$temp_start), max(data$temp_end), by = "day"))
    
    setkey(data, temp_start, temp_end)
    result <- foverlaps(all_dates, data, by.x = c("temp_start", "temp_end"))
    result <- result[, .(N = .N), by = temp_start]
    results_list[[1]] <- result
  } else {
    for (group in unique(data[[group_col]])) {
      subset_data = data[get(group_col) == group]
      all_dates <- data.table(temp_start = seq(min(subset_data$temp_start), max(subset_data$temp_end), by = "day"),
                              temp_end = seq(min(subset_data$temp_start), max(subset_data$temp_end), by = "day"))
      
      setkey(subset_data, temp_start, temp_end)
      result <- foverlaps(all_dates, subset_data, by.x = c("temp_start", "temp_end"))
      result <- result[, .(N = .N, Group = group), by = temp_start]
      results_list[[group]] <- result
    }
  }
  
  final_result = rbindlist(results_list)
  final_result[is.na(N), N := 0L]
  
  # Revert column names
  setnames(final_result, "temp_start", start_col)
  return(final_result)
}

#' Plot Daily Counts
#'
#' This function plots daily counts from a given dataset. It can either plot overall daily counts or grouped daily counts based on a specified group column.
#'
#' @param result The dataset containing the daily counts.
#' @param group_col The column name indicating the groups. Default is \code{NULL}.
#'
#' @return A ggplot2 object representing the plot of daily counts.
#'
#' @importFrom ggplot2 ggplot aes_string theme_minimal geom_line geom_point labs
#' @importFrom scales hue_pal
#' @importFrom rlang sym
#'
#' @details
#' The function uses the \code{aes_string} function for ggplot, which is becoming deprecated in favor of tidy eval functions from \code{ggplot2}.
#'
#' @examples
#' # Example 1: Plot overall daily counts
#' plot_daily_counts(result)
#'
#' # Example 2: Plot grouped daily counts by a specified column
#' plot_daily_counts(result, group_col = "Group")
plot_daily_counts <- function(result, date_col, group_col = NULL, color = "#0072B2FF", size = 1.5, theme_style = theme_minimal) {
  
  if (is.null(date_col) || !(date_col %in% colnames(result))) {
    stop("date_col not provided or doesn't exist in the result.")
  }
  
  plot_base <- ggplot(result, aes_string(x = date_col, y = "N")) + 
    theme_style() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
  
  if (!is.null(group_col)) {  
    if (!(group_col %in% colnames(result))) {
      stop("group_col doesn't exist in the result.")
    }
    
    groups <- unique(result[[group_col]])
    num_groups <- length(groups)
    color_palette <- hue_pal()(num_groups)
    
    plot_base + 
      geom_line(aes_string(group = group_col, color = group_col), linetype = "solid", linewidth = size) +
      geom_point(aes_string(fill = group_col), size = 3, shape = 21) +
      scale_color_manual(values = color_palette) +
      labs(title = "Daily Counts by Group")
  } else {
    plot_base +
      geom_line(color = color, linetype = "solid", linewidth = size) +
      geom_point(color = color, size = 3, shape = 21, fill = "white") +
      labs(title = "Overall Daily Counts")
  }
}