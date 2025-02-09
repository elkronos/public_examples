# Enhanced Daily Counts and Plot Functions with UAT
# =================================================

# Load required libraries
library(data.table)
library(ggplot2)
library(scales)
library(rlang)
library(testthat)  # For the UAT

# ------------------------------------------------------------------------------
# Function: daily_counts
# ------------------------------------------------------------------------------
#' Calculate Daily Counts
#'
#' This function calculates the daily counts of records spanning across multiple 
#' days based on start and end dates provided in a data.table. Optionally, it can 
#' segment the counts by a grouping column.
#'
#' @param data A data.table (or data.frame) containing the data.
#' @param start_col A character string specifying the column name for the start dates.
#' @param end_col A character string specifying the column name for the end dates.
#' @param group_col A character string specifying the column name for grouping 
#'   the data (optional).
#'
#' @return A data.table object with daily counts. The date column is named 
#'   according to \code{start_col} and a count column \code{N} is provided. If 
#'   grouping is used, a column with the same name as \code{group_col} is included.
#'
#' @examples
#' # Without grouping:
#' dt <- data.table(
#'   start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05")),
#'   end   = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08"))
#' )
#' daily_counts(dt, "start", "end")
#'
#' # With grouping:
#' dt_group <- data.table(
#'   start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05", "2023-01-03")),
#'   end   = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08", "2023-01-04")),
#'   group = c("A", "A", "B", "C")
#' )
#' daily_counts(dt_group, "start", "end", group_col = "group")
daily_counts <- function(data, start_col, end_col, group_col = NULL) {
  # Ensure data is a data.table
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }
  
  # Validate required columns exist
  if (!(start_col %in% names(data))) {
    stop("The start_col '", start_col, "' was not found in the data.")
  }
  if (!(end_col %in% names(data))) {
    stop("The end_col '", end_col, "' was not found in the data.")
  }
  if (!is.null(group_col) && !(group_col %in% names(data))) {
    stop("The group_col '", group_col, "' was not found in the data.")
  }
  
  # Check that the data is non-empty
  if (nrow(data) == 0) {
    stop("Input data is empty.")
  }
  
  # Convert start and end columns to Date (if not already)
  data[, temp_start := as.Date(get(start_col))]
  data[, temp_end   := as.Date(get(end_col))]
  
  # Validate that start dates are not later than end dates
  if (any(data$temp_start > data$temp_end, na.rm = TRUE)) {
    stop("Start dates must be less than or equal to end dates.")
  }
  
  results_list <- list()
  
  # Helper function to generate counts for a given data subset
  get_counts <- function(dt_subset) {
    # Determine overall range for the dates
    range_start <- min(dt_subset$temp_start, na.rm = TRUE)
    range_end   <- max(dt_subset$temp_end, na.rm = TRUE)
    # Create a table of all dates in the range (using an interval where start=end)
    all_dates <- data.table(date = seq(range_start, range_end, by = "day"))
    all_dates[, `:=`(temp_start = date, temp_end = date)]
    
    # Set keys for overlap join
    setkey(dt_subset, temp_start, temp_end)
    setkey(all_dates, temp_start, temp_end)
    
    # Use foverlaps to join intervals (any overlap)
    overlaps <- foverlaps(all_dates, dt_subset, type = "any", nomatch = 0L)
    
    # Count occurrences per day and merge with all_dates to include zero counts
    counts <- overlaps[, .(N = .N), by = date]
    counts <- merge(all_dates[, .(date)], counts, by = "date", all.x = TRUE)
    counts[is.na(N), N := 0L]
    
    return(counts)
  }
  
  if (is.null(group_col)) {
    # Without grouping: use all data
    counts <- get_counts(data)
    setnames(counts, "date", start_col)
    return(counts[])
  } else {
    # With grouping: process each group separately (handle NA groups explicitly)
    all_groups <- unique(data[[group_col]])
    for (grp in all_groups) {
      if (is.na(grp)) {
        dt_grp <- data[is.na(get(group_col))]
      } else {
        dt_grp <- data[get(group_col) == grp]
      }
      
      if (nrow(dt_grp) == 0) next  # Skip if no data for this group
      
      counts_grp <- get_counts(dt_grp)
      # Assign the grouping column with the original group value
      counts_grp[, (group_col) := grp]
      results_list[[as.character(grp)]] <- counts_grp
    }
    final_result <- rbindlist(results_list)
    setnames(final_result, "date", start_col)
    return(final_result[])
  }
}

# ------------------------------------------------------------------------------
# Function: plot_daily_counts
# ------------------------------------------------------------------------------
#' Plot Daily Counts
#'
#' This function plots daily counts from a given dataset. It can either plot overall 
#' daily counts or grouped daily counts based on a specified group column.
#'
#' @param result A data.frame or data.table containing the daily counts (including 
#'   a count column named \code{N}).
#' @param date_col A character string specifying the date column name in \code{result}.
#' @param group_col A character string indicating the group column in \code{result} (optional).
#' @param color A color (or hex code) for plotting the overall counts. Default is "#0072B2FF".
#' @param size A numeric value specifying the line width in the plot. Default is 1.5.
#' @param theme_style A ggplot2 theme object to style the plot. Default is \code{theme_minimal()}.
#'
#' @return A ggplot2 object representing the plot of daily counts.
#'
#' @examples
#' # Without grouping:
#' dt <- data.table(
#'   start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05")),
#'   end   = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08"))
#' )
#' counts <- daily_counts(dt, "start", "end")
#' plot_daily_counts(counts, date_col = "start")
#'
#' # With grouping:
#' dt_group <- data.table(
#'   start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05", "2023-01-03")),
#'   end   = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08", "2023-01-04")),
#'   group = c("A", "A", "B", "C")
#' )
#' counts_group <- daily_counts(dt_group, "start", "end", group_col = "group")
#' plot_daily_counts(counts_group, date_col = "start", group_col = "group")
plot_daily_counts <- function(result, date_col, group_col = NULL,
                              color = "#0072B2FF", size = 1.5,
                              theme_style = theme_minimal()) {
  # Validate that the required columns exist in the result
  if (!(date_col %in% names(result))) {
    stop("The date_col '", date_col, "' does not exist in the result.")
  }
  if (!("N" %in% names(result))) {
    stop("The result must contain a column named 'N' representing counts.")
  }
  if (!is.null(group_col) && !(group_col %in% names(result))) {
    stop("The group_col '", group_col, "' does not exist in the result.")
  }
  
  # Ensure the date column is of class Date
  if (!inherits(result[[date_col]], "Date")) {
    result[[date_col]] <- as.Date(result[[date_col]])
  }
  
  # Build the plot using modern ggplot2 syntax
  if (!is.null(group_col)) {
    p <- ggplot(result, aes(x = .data[[date_col]], y = N,
                            group = .data[[group_col]], color = .data[[group_col]])) +
      geom_line(linetype = "solid", linewidth = size) +
      geom_point(size = 3, shape = 21, fill = "white") +
      labs(title = "Daily Counts by Group",
           x = date_col,
           y = "Count",
           color = group_col) +
      theme_style
  } else {
    p <- ggplot(result, aes(x = .data[[date_col]], y = N)) +
      geom_line(color = color, linetype = "solid", linewidth = size) +
      geom_point(color = color, size = 3, shape = 21, fill = "white") +
      labs(title = "Overall Daily Counts",
           x = date_col,
           y = "Count") +
      theme_style
  }
  
  return(p)
}

# ------------------------------------------------------------------------------
# User Acceptance Testing (UAT) Script using testthat
# ------------------------------------------------------------------------------
# To run these tests, you can source this file or use testthat in your project.
# Make sure that testthat is installed.

# Create sample datasets for testing
dt1 <- data.table(
  start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05")),
  end   = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08"))
)

dt2 <- data.table(
  start = as.Date(c("2023-01-02", "2023-01-03", "2023-01-05", "2023-01-03")),
  end   = as.Date(c("2023-01-06", "2023-01-07", "2023-01-08", "2023-01-04")),
  group = c("A", "A", "B", "C")
)

dt3 <- data.table(
  start = as.Date(c("2023-01-01", "2023-01-02")),
  end   = as.Date(c("2023-01-03", "2023-01-04")),
  group = c("A", NA)
)

dt_bad <- data.table(
  start = as.Date("2023-01-10"),
  end   = as.Date("2023-01-05")
)

# ----- Tests for daily_counts -----
test_that("daily_counts without grouping works", {
  res <- daily_counts(dt1, "start", "end")
  expect_true(is.data.table(res))
  expect_true(all(c("start", "N") %in% names(res)))
  
  # Check that the dates span from min(start) to max(end)
  expected_dates <- seq(min(dt1$start), max(dt1$end), by = "day")
  expect_equal(sort(res[[ "start" ]]), expected_dates)
  
  # Ensure counts are nonnegative
  expect_true(all(res$N >= 0))
})

test_that("daily_counts with grouping works", {
  res <- daily_counts(dt2, "start", "end", group_col = "group")
  expect_true(is.data.table(res))
  expect_true(all(c("start", "group", "N") %in% names(res)))
  
  # All groups in original data should be present in the results
  expect_true(all(unique(dt2$group) %in% res$group))
})

test_that("daily_counts handles NA in grouping", {
  res <- daily_counts(dt3, "start", "end", group_col = "group")
  expect_true(is.data.table(res))
  expect_true("group" %in% names(res))
  # Ensure that NA appears as one of the groups
  expect_true(any(is.na(res$group)))
})

test_that("daily_counts errors when start > end", {
  expect_error(daily_counts(dt_bad, "start", "end"),
               "Start dates must be less than or equal to end dates")
})

test_that("daily_counts errors when required column is missing", {
  expect_error(daily_counts(dt1, "nonexistent", "end"),
               "The start_col 'nonexistent' was not found")
  expect_error(daily_counts(dt1, "start", "nonexistent"),
               "The end_col 'nonexistent' was not found")
  expect_error(daily_counts(dt1, "start", "end", group_col = "nonexistent"),
               "The group_col 'nonexistent' was not found")
})

test_that("daily_counts errors when data is empty", {
  dt_empty <- data.table(start = as.Date(character()), end = as.Date(character()))
  expect_error(daily_counts(dt_empty, "start", "end"),
               "Input data is empty")
})

# ----- Tests for plot_daily_counts -----
test_that("plot_daily_counts without grouping returns a ggplot object", {
  res <- daily_counts(dt1, "start", "end")
  p <- plot_daily_counts(res, date_col = "start")
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_daily_counts with grouping returns a ggplot object", {
  res <- daily_counts(dt2, "start", "end", group_col = "group")
  p <- plot_daily_counts(res, date_col = "start", group_col = "group")
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_daily_counts errors when date_col is missing", {
  # Rename column to simulate a missing date_col
  dt_wrong <- copy(dt1)
  setnames(dt_wrong, "start", "wrong_date")
  expect_error(plot_daily_counts(dt_wrong, date_col = "start"),
               "The date_col 'start' does not exist")
})

test_that("plot_daily_counts errors when group_col is missing", {
  res <- daily_counts(dt1, "start", "end")
  expect_error(plot_daily_counts(res, date_col = "start", group_col = "group"),
               "The group_col 'group' does not exist")
})

# Print a message if all tests pass
print("All tests passed!")
