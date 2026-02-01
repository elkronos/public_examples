# Load required libraries
library(ggplot2)
library(dplyr)
library(scales)
library(zoo)
library(lubridate)
library(testthat)

#' bar_time: Time Series Bar Plot with Rolling Line
#'
#' Creates a time series bar plot with an optional rolling line computed from aggregated values.
#'
#' @param data A data frame containing the time series data.
#' @param date_col A single character string naming the date/datetime column.
#' @param y_col A single character string naming the numeric value column.
#' @param group_by A single character string specifying the aggregation interval:
#'   "day", "week", "month", "quarter", or "year".
#' @param window_size A single numeric value >= 1 specifying the rolling window width.
#' @param bar_stat Either a built-in aggregation name ("sum", "mean", "median") or a function.
#' @param line_stat Either a built-in rolling statistic name ("mean", "median", "max", "min") or a function.
#'
#' @return A ggplot object.
#' @export
bar_time <- function(
    data,
    date_col,
    y_col,
    group_by = "day",
    window_size = 7,
    bar_stat = "sum",
    line_stat = "mean"
) {
  # Argument checks
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (!is.character(date_col) || length(date_col) != 1) stop("'date_col' must be a single character string.")
  if (!is.character(y_col) || length(y_col) != 1) stop("'y_col' must be a single character string.")
  if (!is.character(group_by) || length(group_by) != 1) stop("'group_by' must be a single character string.")
  
  allowed_groups <- c("day", "week", "month", "quarter", "year")
  if (!group_by %in% allowed_groups) {
    stop(sprintf("Invalid 'group_by'. Allowed values: %s", paste(allowed_groups, collapse = ", ")))
  }
  
  if (!is.numeric(window_size) || length(window_size) != 1 || is.na(window_size) || window_size < 1) {
    stop("'window_size' must be a single numeric value >= 1.")
  }
  window_size <- as.integer(window_size)
  
  if (!date_col %in% names(data)) stop(sprintf("The column %s is not present in the data.", date_col))
  if (!y_col %in% names(data)) stop(sprintf("The column %s is not present in the data.", y_col))
  
  # Statistic resolution
  resolve_stat <- function(stat, allowed, name) {
    if (is.character(stat)) {
      if (length(stat) != 1 || !stat %in% allowed) {
        stop(sprintf(
          "Invalid '%s'. Allowed character values: %s",
          name, paste(allowed, collapse = ", ")
        ))
      }
      fn <- switch(
        stat,
        sum = sum,
        mean = mean,
        median = median,
        max = max,
        min = min
      )
      return(list(fn = fn, label = stat))
    }
    
    if (is.function(stat)) {
      return(list(fn = stat, label = "custom"))
    }
    
    stop(sprintf("'%s' must be either a character string or a function.", name))
  }
  
  bar_res <- resolve_stat(bar_stat, c("sum", "mean", "median"), "bar_stat")
  line_res <- resolve_stat(line_stat, c("mean", "median", "max", "min"), "line_stat")
  
  # Function wrappers for NA handling
  call_with_na_rm <- function(fn, x) {
    fmls <- formals(fn)
    if (!is.null(fmls) && ("na.rm" %in% names(fmls))) return(fn(x, na.rm = TRUE))
    fn(x)
  }
  
  bar_fn <- function(x) call_with_na_rm(bar_res$fn, x)
  line_fn <- function(x) call_with_na_rm(line_res$fn, x)
  
  # Column preparation
  raw_date <- data[[date_col]]
  raw_y <- data[[y_col]]
  
  date_vec <- if (inherits(raw_date, "Date")) {
    raw_date
  } else {
    as.Date(raw_date)
  }
  
  if (all(is.na(date_vec))) {
    stop("Date conversion produced all NA values. Provide a Date/POSIXt column or values coercible by as.Date().")
  }
  
  y_vec <- suppressWarnings(as.numeric(raw_y))
  if (all(is.na(y_vec))) {
    stop("Numeric conversion produced all NA values. Provide a numeric column or values coercible by as.numeric().")
  }
  
  # Grouping key computation
  group_date <- switch(
    group_by,
    day = date_vec,
    week = lubridate::floor_date(date_vec, unit = "week", week_start = 1),
    month = lubridate::floor_date(date_vec, unit = "month"),
    quarter = lubridate::floor_date(date_vec, unit = "quarter"),
    year = lubridate::floor_date(date_vec, unit = "year")
  )
  
  dropped_dates <- sum(is.na(group_date))
  if (dropped_dates > 0) {
    warning(sprintf("Removed %d rows with missing dates after conversion/grouping.", dropped_dates))
  }
  
  df <- tibble::tibble(group_date = group_date, y = y_vec) %>%
    filter(!is.na(group_date))
  
  # Aggregation
  data_summary <- df %>%
    group_by(group_date) %>%
    summarize(bar_value = bar_fn(y), .groups = "drop") %>%
    arrange(group_date)
  
  # Rolling statistic on aggregated values
  if (nrow(data_summary) < window_size) {
    warning("Window size is larger than the number of groups; moving statistic will be NA for all groups.")
    data_summary <- mutate(data_summary, moving_value = NA_real_)
  } else {
    data_summary <- mutate(
      data_summary,
      moving_value = zoo::rollapplyr(
        bar_value,
        width = window_size,
        FUN = line_fn,
        fill = NA_real_
      )
    )
  }
  
  # Scale formatting
  break_width <- switch(
    group_by,
    day = "1 month",
    week = "1 month",
    month = "3 months",
    quarter = "6 months",
    year = "1 year"
  )
  
  label_fn <- switch(
    group_by,
    day = scales::label_date("%Y-%m-%d"),
    week = scales::label_date("%Y-%m-%d"),
    month = scales::label_date("%Y-%m"),
    year = scales::label_date("%Y"),
    quarter = function(x) paste0(lubridate::year(x), "-Q", lubridate::quarter(x))
  )
  
  # Plot construction
  ggplot(data_summary, aes(x = group_date)) +
    geom_col(aes(y = bar_value), fill = "steelblue", alpha = 0.8) +
    geom_line(aes(y = moving_value), color = "red", linewidth = 1, na.rm = TRUE) +
    scale_x_date(breaks = scales::breaks_width(break_width), labels = label_fn) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      x = "Date",
      y = paste("Bar", bar_res$label),
      title = paste0(
        "Time Series Bars (", bar_res$label, ") with Rolling ", line_res$label,
        " (window=", window_size, ")"
      )
    )
}

# ---------------------------------------------------------------------
# User Acceptance Tests (UAT) using testthat
# ---------------------------------------------------------------------

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
  p <- bar_time(
    test_data,
    date_col = "date",
    y_col = "value",
    group_by = "week",
    window_size = 4,
    bar_stat = "mean",
    line_stat = "median"
  )
  expect_s3_class(p, "ggplot")
})

test_that("Month grouping with median bar and max line produces a ggplot", {
  p <- bar_time(
    test_data,
    date_col = "date",
    y_col = "value",
    group_by = "month",
    window_size = 2,
    bar_stat = "median",
    line_stat = "max"
  )
  expect_s3_class(p, "ggplot")
})

test_that("Quarter grouping with sum bar and min line produces a ggplot", {
  p <- bar_time(
    test_data,
    date_col = "date",
    y_col = "value",
    group_by = "quarter",
    window_size = 2,
    bar_stat = "sum",
    line_stat = "min"
  )
  expect_s3_class(p, "ggplot")
})

test_that("Year grouping with mean bar and mean line produces a ggplot", {
  p <- bar_time(
    test_data,
    date_col = "date",
    y_col = "value",
    group_by = "year",
    window_size = 1,
    bar_stat = "mean",
    line_stat = "mean"
  )
  expect_s3_class(p, "ggplot")
})

test_that("Window size larger than group count issues warning", {
  expect_warning(
    p <- bar_time(
      test_data,
      date_col = "date",
      y_col = "value",
      group_by = "month",
      window_size = 20,
      bar_stat = "sum",
      line_stat = "max"
    ),
    "Window size is larger than the number of groups"
  )
  expect_s3_class(p, "ggplot")
})

test_that("Missing values in y_col are handled", {
  test_data_na <- test_data
  test_data_na$value[sample.int(nrow(test_data_na), 20)] <- NA
  p <- bar_time(
    test_data_na,
    date_col = "date",
    y_col = "value",
    group_by = "month",
    window_size = 3,
    bar_stat = "mean",
    line_stat = "mean"
  )
  expect_s3_class(p, "ggplot")
})

test_that("Invalid group_by triggers error", {
  expect_error(
    bar_time(test_data, date_col = "date", y_col = "value", group_by = "invalid"),
    "Invalid 'group_by'"
  )
})

test_that("Invalid bar_stat triggers error", {
  expect_error(
    bar_time(test_data, date_col = "date", y_col = "value", bar_stat = "invalid"),
    "Invalid 'bar_stat'"
  )
})

test_that("Invalid line_stat triggers error", {
  expect_error(
    bar_time(test_data, date_col = "date", y_col = "value", line_stat = "invalid"),
    "Invalid 'line_stat'"
  )
})

test_that("Missing date column triggers error", {
  expect_error(
    bar_time(test_data, date_col = "nonexistent", y_col = "value"),
    "is not present in the data"
  )
})

test_that("Missing y column triggers error", {
  expect_error(
    bar_time(test_data, date_col = "date", y_col = "nonexistent"),
    "is not present in the data"
  )
})

test_that("Custom functions for bar_stat and line_stat work", {
  p <- bar_time(
    test_data,
    date_col = "date",
    y_col = "value",
    group_by = "month",
    window_size = 2,
    bar_stat = function(x) stats::quantile(x, 0.75, na.rm = TRUE),
    line_stat = function(x) stats::sd(x, na.rm = TRUE)
  )
  expect_s3_class(p, "ggplot")
})

test_that("POSIXct date columns are supported", {
  td <- test_data
  td$date_time <- as.POSIXct(td$date)
  p <- bar_time(td, date_col = "date_time", y_col = "value", group_by = "week", window_size = 3)
  expect_s3_class(p, "ggplot")
})
