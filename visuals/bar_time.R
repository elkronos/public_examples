# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(zoo)
library(lubridate)
library(testthat)

#' bar_time: Time Series Bar Plot with Moving Line
#'
#' This function creates a time series bar plot with a moving line based on aggregated data.
#' It allows for grouping the data by different time intervals such as day, week, month, quarter, or year,
#' and calculates the aggregated values for the bars and the moving line.
#'
#' @param data A data frame containing the time series data.
#' @param date_col A character string specifying the name of the date column in \code{data}.
#' @param y_col A character string specifying the name of the numeric value column in \code{data}.
#' @param group_by A character string specifying the time interval for grouping the data.
#'   Allowed values are \code{"day"}, \code{"week"}, \code{"month"}, \code{"quarter"}, or \code{"year"}.
#'   The default is \code{"day"}.
#' @param window_size An integer specifying the size of the moving window used to calculate the moving line.
#'   Must be greater than or equal to 1. The default is 7.
#' @param bar_stat Either a character string specifying a built-in aggregation statistic for the bar heights
#'   (allowed values are \code{"sum"}, \code{"mean"}, or \code{"median"}) or a custom function that
#'   accepts a numeric vector and returns a single numeric value. The default is \code{"sum"}.
#' @param line_stat Either a character string specifying a built-in statistic for the moving line
#'   (allowed values are \code{"mean"}, \code{"median"}, \code{"max"}, or \code{"min"}) or a custom function that
#'   accepts a numeric vector and returns a single numeric value. The default is \code{"mean"}.
#'
#' @return A \code{ggplot} object representing the time series bar plot with the moving line.
#'
#' @details
#' The function converts the specified date and numeric columns to appropriate types and then groups the data
#' according to the specified time interval. It aggregates the grouped data using the function specified by
#' \code{bar_stat} and computes a rolling (moving) statistic using \code{line_stat} over a window of size
#' \code{window_size}. When custom functions are provided, they should accept a numeric vector and, optionally,
#' a \code{na.rm} argument. If the custom bar function accepts a \code{na.rm} parameter, it will be passed
#' \code{na.rm = TRUE}; otherwise, it will be called with just the numeric vector.
#'
#' @examples
#' \dontrun{
#' # Example using built-in statistics:
#' set.seed(123)
#' data <- data.frame(
#'   date = seq(as.Date("2010-01-01"), as.Date("2010-12-31"), by = "day"),
#'   value = rnorm(365, 10, 2)
#' )
#'
#' # Default usage: daily grouping, sum for bars, mean for moving line
#' p1 <- bar_time(data, date_col = "date", y_col = "value")
#' print(p1)
#'
#' # Monthly grouping with median bars and max for the moving line
#' p2 <- bar_time(data, date_col = "date", y_col = "value",
#'                group_by = "month", window_size = 2, bar_stat = "median", line_stat = "max")
#' print(p2)
#'
#' # Example using custom functions:
#' p3 <- bar_time(data, date_col = "date", y_col = "value",
#'                group_by = "month", window_size = 2,
#'                bar_stat = function(x) quantile(x, 0.75, na.rm = TRUE),
#'                line_stat = function(x) sd(x, na.rm = TRUE))
#' print(p3)
#' }
#'
#' @import ggplot2 dplyr scales zoo lubridate
#' @export

bar_time <- function(data, date_col, y_col, group_by = "day", window_size = 7,
                     bar_stat = "sum", line_stat = "mean") {
  
  ## --- Input Validation ---
  
  # Check data type
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.")
  }
  
  # Check that date_col, y_col, and group_by are single character strings
  if (!is.character(date_col) || length(date_col) != 1) {
    stop("The 'date_col' argument must be a single character string.")
  }
  if (!is.character(y_col) || length(y_col) != 1) {
    stop("The 'y_col' argument must be a single character string.")
  }
  if (!is.character(group_by) || length(group_by) != 1) {
    stop("The 'group_by' argument must be a single character string.")
  }
  
  # Check window_size
  if (!is.numeric(window_size) || length(window_size) != 1 || window_size < 1) {
    stop("The 'window_size' argument must be a single numeric value greater than or equal to 1.")
  }
  
  # Allowed group_by values
  allowed_groups <- c("day", "week", "month", "quarter", "year")
  if (!(group_by %in% allowed_groups)) {
    stop(paste("Invalid 'group_by' argument. Allowed values are:",
               paste(allowed_groups, collapse = ", ")))
  }
  
  # Determine bar function: either a character string or a custom function.
  if (is.character(bar_stat)) {
    allowed_bar_stats <- c("sum", "mean", "median")
    if (!(bar_stat %in% allowed_bar_stats)) {
      stop(paste("Invalid 'bar_stat' argument. Allowed character values are:",
                 paste(allowed_bar_stats, collapse = ", ")))
    }
    bar_fun <- switch(bar_stat,
                      sum = sum,
                      mean = mean,
                      median = median)
  } else if (is.function(bar_stat)) {
    bar_fun <- bar_stat
  } else {
    stop("bar_stat must be either a character string or a function.")
  }
  
  # Determine line function: either a character string or a custom function.
  if (is.character(line_stat)) {
    allowed_line_stats <- c("mean", "median", "max", "min")
    if (!(line_stat %in% allowed_line_stats)) {
      stop(paste("Invalid 'line_stat' argument. Allowed character values are:",
                 paste(allowed_line_stats, collapse = ", ")))
    }
    line_fun <- switch(line_stat,
                       mean = mean,
                       median = median,
                       max = max,
                       min = min)
  } else if (is.function(line_stat)) {
    line_fun <- line_stat
  } else {
    stop("line_stat must be either a character string or a function.")
  }
  
  # Check that the specified columns exist in the data
  if (!(date_col %in% names(data))) {
    stop(paste("The column", date_col, "is not present in the data."))
  }
  if (!(y_col %in% names(data))) {
    stop(paste("The column", y_col, "is not present in the data."))
  }
  
  ## --- Data Preparation ---
  
  # Convert the date column to Date format
  data <- data %>% mutate(.date = as.Date(.data[[date_col]]))
  if (all(is.na(data$.date))) {
    stop("Conversion of 'date_col' to Date resulted in all NA values. Please check the date format.")
  }
  
  # Convert the y column to numeric
  data <- data %>% mutate(.y = as.numeric(.data[[y_col]]))
  if (all(is.na(data$.y))) {
    stop("Conversion of 'y_col' to numeric resulted in all NA values. Please check the values.")
  }
  
  # Create a grouping date based on the desired time interval
  data <- data %>% mutate(group_date = switch(
    group_by,
    day = .date,
    week = floor_date(.date, unit = "week", week_start = 1),
    month = floor_date(.date, unit = "month"),
    quarter = floor_date(.date, unit = "quarter"),
    year = floor_date(.date, unit = "year")
  ))
  
  ## --- Aggregation ---
  
  # Define a helper that calls the bar function
  call_bar <- function(x) {
    if ("na.rm" %in% names(formals(bar_fun))) {
      bar_fun(x, na.rm = TRUE)
    } else {
      bar_fun(x)
    }
  }
  
  # Aggregate the data by the grouping date using the selected bar function
  data_summary <- data %>%
    group_by(group_date) %>%
    summarize(bar_value = call_bar(.y), .groups = "drop") %>%
    arrange(group_date)
  
  ## --- Rolling Statistic (Moving Line) ---
  
  # If the number of groups is smaller than the window size, warn and set moving_value to NA
  if(nrow(data_summary) < window_size) {
    warning("Window size is larger than the number of groups; moving statistic will be NA for all groups.")
    data_summary <- data_summary %>% mutate(moving_value = NA_real_)
  } else {
    # Calculate the moving statistic over the aggregated bar values
    data_summary <- data_summary %>%
      mutate(moving_value = zoo::rollapply(bar_value, width = window_size,
                                           FUN = line_fun, align = "right", fill = NA))
  }
  
  ## --- Plot Construction ---
  
  # Set up date formatting for the x-axis based on the grouping interval
  date_format_string <- switch(group_by,
                               day = "%Y-%m-%d",
                               week = "%Y-%m-%d",
                               month = "%Y-%m",
                               quarter = "%Y-Q%q",
                               year = "%Y")
  
  # Choose a reasonable date break interval
  date_break_interval <- switch(group_by,
                                day = "1 month",
                                week = "1 month",
                                month = "3 months",
                                quarter = "6 months",
                                year = "1 year")
  
  # Build and return the ggplot
  p <- ggplot(data_summary, aes(x = group_date)) +
    geom_bar(aes(y = bar_value), stat = "identity", fill = "steelblue", alpha = 0.8) +
    geom_line(aes(y = moving_value), color = "red", linewidth = 1, na.rm = TRUE) +
    scale_x_date(labels = date_format(date_format_string), breaks = date_breaks(date_break_interval)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Date",
         y = paste("Bar", ifelse(is.character(bar_stat), bar_stat, "custom")),
         title = paste("Time Series Bar Plot (", ifelse(is.character(bar_stat), bar_stat, "custom"),
                       ") with Rolling", ifelse(is.character(line_stat), line_stat, "custom")))
  
  return(p)
}

# ---------------------------------------------------------------------
# User Acceptance Tests (UAT) using testthat
# ---------------------------------------------------------------------

context("Testing bar_time Function")

# Create test data
set.seed(123)
test_data <- data.frame(
  date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
  value = rnorm(366, mean = 50, sd = 10)
)

test_that("Default parameters produce a ggplot object", {
  p <- bar_time(test_data, date_col = "date", y_col = "value")
  expect_s3_class(p, "ggplot")
})

test_that("Week grouping with mean bar and median line produces a ggplot", {
  p <- bar_time(test_data, date_col = "date", y_col = "value",
                group_by = "week", window_size = 4, bar_stat = "mean", line_stat = "median")
  expect_s3_class(p, "ggplot")
})

test_that("Month grouping with median bar and max line produces a ggplot", {
  p <- bar_time(test_data, date_col = "date", y_col = "value",
                group_by = "month", window_size = 2, bar_stat = "median", line_stat = "max")
  expect_s3_class(p, "ggplot")
})

test_that("Quarter grouping with sum bar and min line produces a ggplot", {
  p <- bar_time(test_data, date_col = "date", y_col = "value",
                group_by = "quarter", window_size = 2, bar_stat = "sum", line_stat = "min")
  expect_s3_class(p, "ggplot")
})

test_that("Year grouping with mean bar and mean line produces a ggplot", {
  p <- bar_time(test_data, date_col = "date", y_col = "value",
                group_by = "year", window_size = 1, bar_stat = "mean", line_stat = "mean")
  expect_s3_class(p, "ggplot")
})

test_that("Edge case: window_size larger than number of groups issues warning", {
  expect_warning(
    p <- bar_time(test_data, date_col = "date", y_col = "value",
                  group_by = "month", window_size = 20, bar_stat = "sum", line_stat = "max"),
    "Window size is larger than the number of groups"
  )
  expect_s3_class(p, "ggplot")
})

test_that("Missing values in y_col are handled correctly", {
  test_data_na <- test_data
  test_data_na$value[sample(1:nrow(test_data_na), 20)] <- NA
  p <- bar_time(test_data_na, date_col = "date", y_col = "value",
                group_by = "month", window_size = 3, bar_stat = "mean", line_stat = "mean")
  expect_s3_class(p, "ggplot")
})

test_that("Error is thrown for invalid group_by value", {
  expect_error(
    bar_time(test_data, date_col = "date", y_col = "value", group_by = "invalid",
             window_size = 7, bar_stat = "sum", line_stat = "mean"),
    "Invalid 'group_by' argument"
  )
})

test_that("Error is thrown for invalid bar_stat value", {
  expect_error(
    bar_time(test_data, date_col = "date", y_col = "value", group_by = "day",
             window_size = 7, bar_stat = "invalid", line_stat = "mean"),
    "Invalid 'bar_stat' argument"
  )
})

test_that("Error is thrown for invalid line_stat value", {
  expect_error(
    bar_time(test_data, date_col = "date", y_col = "value", group_by = "day",
             window_size = 7, bar_stat = "sum", line_stat = "invalid"),
    "Invalid 'line_stat' argument"
  )
})

test_that("Error is thrown for missing date column", {
  expect_error(
    bar_time(test_data, date_col = "nonexistent", y_col = "value"),
    "The column nonexistent is not present in the data."
  )
})

test_that("Error is thrown for missing y column", {
  expect_error(
    bar_time(test_data, date_col = "date", y_col = "nonexistent"),
    "The column nonexistent is not present in the data."
  )
})

test_that("Custom functions for bar_stat and line_stat work", {
  p <- bar_time(test_data, date_col = "date", y_col = "value",
                group_by = "month", window_size = 2,
                bar_stat = function(x) quantile(x, 0.75, na.rm = TRUE),
                line_stat = function(x) sd(x, na.rm = TRUE))
  expect_s3_class(p, "ggplot")
})

# To run the tests, you can use test_dir() if these tests are in a directory,
# or test_file("this_file.R") if they are saved in a file.
# For example, if saved in "bar_time_tests.R":
# test_file("bar_time_tests.R")
