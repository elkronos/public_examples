# Enhanced Daily Summary Module with Full UAT
library(data.table)
library(ggplot2)
library(testthat)

#' Expand Date Ranges
#'
#' This helper function takes a data.table with start and end dates and expands each row
#' into one row per day in the range. It also replicates the associated numeric value.
#'
#' @param dt A data.table containing the data.
#' @param start_col A character string with the name of the start date column.
#' @param end_col A character string with the name of the end date column.
#' @param value_col A character string with the name of the numeric value column.
#' @param group_col An optional character string specifying a grouping column.
#'
#' @return A data.table with columns "Date" and "Value" (plus the group column if provided).
expand_date_ranges <- function(dt, start_col, end_col, value_col, group_col = NULL) {
  # Ensure that the start and end columns are of class Date
  dt[, (start_col) := as.Date(get(start_col))]
  dt[, (end_col)   := as.Date(get(end_col))]
  
  # Validate: no missing dates
  if (dt[is.na(get(start_col)) | is.na(get(end_col)), .N] > 0) {
    stop("One or more rows have NA in start or end dates.")
  }
  
  # Validate: each start must be <= its corresponding end
  if (dt[get(start_col) > get(end_col), .N] > 0) {
    stop("One or more rows have a start date later than the end date.")
  }
  
  # Row-by-row expansion: iterate over each row using lapply()
  if (is.null(group_col)) {
    out_list <- lapply(seq_len(nrow(dt)), function(i) {
      start_val <- dt[[start_col]][i]
      end_val   <- dt[[end_col]][i]
      value_val <- dt[[value_col]][i]
      data.table(
        Date = seq(start_val, end_val, by = "day"),
        Value = rep(value_val, times = as.integer(end_val - start_val + 1))
      )
    })
  } else {
    out_list <- lapply(seq_len(nrow(dt)), function(i) {
      start_val <- dt[[start_col]][i]
      end_val   <- dt[[end_col]][i]
      value_val <- dt[[value_col]][i]
      group_val <- dt[[group_col]][i]
      data.table(
        # Use a temporary name for the group column
        grp = group_val,
        Date = seq(start_val, end_val, by = "day"),
        Value = rep(value_val, times = as.integer(end_val - start_val + 1))
      )
    })
  }
  
  expanded <- rbindlist(out_list)
  
  # If grouping was used, rename the temporary column to the desired name.
  if (!is.null(group_col)) {
    setnames(expanded, "grp", group_col)
  }
  
  return(expanded)
}

#' Daily Summary
#'
#' This function computes a daily summary statistic (sum, mean, or median) for numeric values
#' that are active over specified date ranges. Optionally, the summary can be computed by group.
#'
#' @param data A data.table containing the dataset.
#' @param start_col A character string specifying the start date column.
#' @param end_col A character string specifying the end date column.
#' @param value_col A character string specifying the numeric value column.
#' @param group_col An optional character string specifying the column to group by.
#' @param summary_func Either a character string ("sum", "mean", or "median") specifying the
#'   summary statistic to compute, or a custom function that takes a numeric vector and returns a number.
#'
#' @return A data.table with one row per day (and per group if group_col is provided) and columns:
#'   - "Date": the date.
#'   - "N": the computed summary statistic.
daily_summary <- function(data, start_col, end_col, value_col, group_col = NULL, summary_func = "sum") {
  # Validate that the input is a data.table
  if (!is.data.table(data)) {
    stop("Input data must be a data.table.")
  }
  
  # Validate that required columns exist
  required_cols <- c(start_col, end_col, value_col)
  if (!all(required_cols %in% names(data))) {
    stop("One or more of the specified columns (start, end, value) do not exist in the input data.")
  }
  
  # Validate that group_col exists (if provided)
  if (!is.null(group_col) && !(group_col %in% names(data))) {
    stop("The specified group column does not exist in the input data.")
  }
  
  # Validate and set the summary function
  if (is.character(summary_func)) {
    allowed_funcs <- c("sum", "mean", "median")
    if (!(summary_func %in% allowed_funcs)) {
      stop("Invalid summary function. Choose 'sum', 'mean', or 'median', or provide a custom function.")
    }
    summary_fun <- switch(summary_func,
                          sum = function(x) sum(x, na.rm = TRUE),
                          mean = function(x) mean(x, na.rm = TRUE),
                          median = function(x) median(x, na.rm = TRUE))
  } else if (is.function(summary_func)) {
    summary_fun <- summary_func
  } else {
    stop("summary_func must be either a character string or a function.")
  }
  
  # Expand the input data to have one row per active date
  expanded_data <- expand_date_ranges(data, start_col, end_col, value_col, group_col)
  
  # Build a complete grid of dates covering the entire range (and groups if applicable)
  min_date <- min(as.Date(data[[start_col]]), na.rm = TRUE)
  max_date <- max(as.Date(data[[end_col]]), na.rm = TRUE)
  all_dates <- seq(min_date, max_date, by = "day")
  
  if (!is.null(group_col)) {
    groups <- unique(data[[group_col]])
    # Create complete grid using a temporary column name, then rename it
    complete_grid <- CJ(tmp = groups, Date = all_dates)
    setnames(complete_grid, "tmp", group_col)
  } else {
    complete_grid <- data.table(Date = all_dates)
  }
  
  # Determine the join columns based on whether group_col is provided
  join_cols <- names(complete_grid)
  
  # Join expanded_data onto the complete grid.
  # For each grid row, compute the summary of the matching 'Value's.
  # If there are no non-missing values, return NA_real_ (to force a numeric result).
  summary_table <- expanded_data[complete_grid, on = join_cols, allow.cartesian = TRUE,
                                 .(N = {
                                   x <- Value
                                   if (length(x) == 0 || all(is.na(x))) NA_real_ else summary_fun(x)
                                 }), by = .EACHI]
  
  # Replace any remaining NA with 0 (if desired; here we mimic previous behavior)
  summary_table[is.na(N), N := 0]
  
  # Order the results for readability using setorderv (which accepts a character vector of column names)
  if (!is.null(group_col)) {
    setorderv(summary_table, c(group_col, "Date"))
  } else {
    setorderv(summary_table, "Date")
  }
  
  return(summary_table)
}

# -------------------------------
# UAT: User Acceptance Testing
# -------------------------------

# Create a sample data.table for testing
sample_data <- data.table(
  group = c("A", "A", "A", "A", "B", "B", "B", "B"),
  start = as.Date(c("1999-01-02", "1999-01-02", "1999-01-03", "1999-01-03",
                    "1999-01-06", "1999-01-06", "1999-01-07", "1999-01-07")),
  end = as.Date(c("1999-01-05", "1999-01-05", "1999-01-06", "1999-01-06",
                  "1999-01-09", "1999-01-09", "1999-01-10", "1999-01-10")),
  value = c(5, 4, 8, 2, 4, 1, 5, 3)
)

# Test 1: Sum summary with grouping
test_that("daily_summary sum with group works correctly", {
  result <- daily_summary(sample_data, "start", "end", "value", "group", "sum")
  expect_true(all(c("group", "Date", "N") %in% names(result)))
  expect_true(all(result$N >= 0))
})

# Test 2: Mean summary with grouping
test_that("daily_summary mean with group works correctly", {
  result <- daily_summary(sample_data, "start", "end", "value", "group", "mean")
  expect_true(all(c("group", "Date", "N") %in% names(result)))
  expect_true(is.numeric(result$N))
})

# Test 3: Median summary with grouping
test_that("daily_summary median with group works correctly", {
  result <- daily_summary(sample_data, "start", "end", "value", "group", "median")
  expect_true(all(c("group", "Date", "N") %in% names(result)))
  expect_true(is.numeric(result$N))
})

# Test 4: Sum summary without grouping
test_that("daily_summary sum without group works correctly", {
  result <- daily_summary(sample_data, "start", "end", "value", group_col = NULL, summary_func = "sum")
  expect_true(all(c("Date", "N") %in% names(result)))
  expect_true(is.numeric(result$N))
})

# Test 5: Custom summary function (example: maximum value)
test_that("daily_summary with custom summary function works correctly", {
  custom_fun <- function(x) {
    if(length(x) == 0 || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  }
  result <- daily_summary(sample_data, "start", "end", "value", "group", custom_fun)
  expect_true(all(c("group", "Date", "N") %in% names(result)))
  # For each group, the maximum value should be no less than any daily summary
  for (g in unique(sample_data$group)) {
    subset_data <- sample_data[group == g]
    expect_true(max(subset_data$value) <= max(result[get("group") == g]$N))
  }
})

# Test 6: Single-day range (edge case)
test_that("daily_summary works for a single-day range", {
  single_day <- data.table(start = as.Date("2020-01-01"), end = as.Date("2020-01-01"), value = 10)
  result <- daily_summary(single_day, "start", "end", "value")
  expect_equal(nrow(result), 1)
  expect_equal(result$N, 10)
})

# Test 7: Error when input is not a data.table
test_that("daily_summary errors with non-data.table input", {
  df <- data.frame(start = as.Date("2020-01-01"), end = as.Date("2020-01-02"), value = 10)
  expect_error(daily_summary(df, "start", "end", "value"), "Input data must be a data.table")
})

# Test 8: Error when required columns are missing
test_that("daily_summary errors when required columns are missing", {
  dt_missing <- copy(sample_data)
  dt_missing[, start := NULL]
  expect_error(daily_summary(dt_missing, "start", "end", "value"), "One or more of the specified columns")
})

# Test 9: Error when the grouping column does not exist
test_that("daily_summary errors when group_col does not exist", {
  expect_error(daily_summary(sample_data, "start", "end", "value", "nonexistent"), 
               "The specified group column does not exist")
})

# Test 10: Error when an invalid summary function is provided
test_that("daily_summary errors for invalid summary_func", {
  expect_error(daily_summary(sample_data, "start", "end", "value", "group", "invalid"),
               "Invalid summary function")
})

cat("All tests passed successfully.\n")

# -------------------------------
# Example Plot Generation
# -------------------------------

# Calculate daily mean and median summaries by group
result_mean   <- daily_summary(sample_data, "start", "end", "value", "group", "mean")
result_median <- daily_summary(sample_data, "start", "end", "value", "group", "median")

# Merge the two summaries for plotting
result_plot <- merge(result_mean, result_median, by = c("group", "Date"), suffixes = c("_mean", "_median"))
result_plot[, Date := as.Date(Date)]

# Create the plot with ggplot2
plot <- ggplot(result_plot, aes(x = Date)) +
  geom_line(aes(y = N_mean, color = group, linetype = "Mean"), linewidth = 1) +
  geom_line(aes(y = N_median, color = group, linetype = "Median"), linewidth = 1) +
  labs(x = "Date", y = "Summary", color = "Group", linetype = "Statistic") +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black", size = 14, face = "bold")
  )

# Display the plot
print(plot)
