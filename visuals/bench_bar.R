#' Benchmark bar plots with threshold line and error bars
#'
#' Construct a bar plot of the mean of a numeric variable (y) for every level of a grouping variable (x).
#' The user can specify a threshold (either computed from the data or manually provided) that is drawn as a
#' horizontal dashed line. Error bars representing either standard errors or standard deviations are added.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_var The unquoted name of the variable to be used on the x-axis (typically a factor or character).
#' @param y_var The unquoted name of the numeric variable to be used on the y-axis.
#' @param color Logical. If TRUE, bars are colored based on whether their mean is above or below the threshold.
#' @param threshold The threshold value for bar coloring. Can be `"mean"`, `"median"`, or a numeric value.
#' @param y_limits A numeric vector of length 2 specifying y-axis limits. If `NULL` (the default), limits are determined automatically.
#' @param colors A character vector of length at least 2 specifying the colors for bars below and above the threshold.
#' @param error_type A character string specifying the type of error bars to plot. Must be either `"se"` (standard error) or `"sd"` (standard deviation).
#' @return A ggplot object.
#' @import ggplot2 dplyr rlang assertthat
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic bar plot with standard errors and mean threshold
#' data <- data.frame(group = rep(c("A", "B", "C"), each = 20),
#'                    value = c(rnorm(20, 10, 2), rnorm(20, 12, 2), rnorm(20, 15, 2)))
#' p <- bench_bar(data, group, value)
#' print(p)
#'
#' # Example 2: Bar plot with custom colors and y-axis limits
#' data <- data.frame(group = rep(c("D", "E", "F"), each = 20),
#'                    value = c(rnorm(20, 50, 10), rnorm(20, 60, 10), rnorm(20, 70, 10)))
#' p2 <- bench_bar(data, group, value, colors = c("blue", "orange"), y_limits = c(40, 80))
#' print(p2)
#'
#' # Example 3: Bar plot with median threshold and standard deviation error bars
#' data <- data.frame(group = rep(c("G", "H", "I"), each = 20),
#'                    value = c(rnorm(20, 5, 1), rnorm(20, 7, 1), rnorm(20, 9, 1)))
#' p3 <- bench_bar(data, group, value, threshold = "median", error_type = "sd")
#' print(p3)
#' }
bench_bar <- function(data, x_var, y_var, color = TRUE, threshold = "mean",
                      y_limits = NULL, colors = c("yellow", "blue"), error_type = "se") {
  # Input checks ---------------------------------------------------------------
  assertthat::assert_that(is.data.frame(data), msg = "data must be a data frame")
  
  # Use tidy evaluation for variable names
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  x_name <- quo_name(x_var)
  y_name <- quo_name(y_var)
  
  assertthat::assert_that(x_name %in% names(data),
                          msg = paste("Column", x_name, "not found in data"))
  assertthat::assert_that(y_name %in% names(data),
                          msg = paste("Column", y_name, "not found in data"))
  
  if (!is.numeric(data[[y_name]])) {
    stop(paste(y_name, "must be numeric"))
  }
  
  if (!is.character(colors) || length(colors) < 2) {
    stop("colors must be a character vector of length at least 2")
  }
  
  if (!is.null(y_limits)) {
    if (!(is.numeric(y_limits) && length(y_limits) == 2)) {
      stop("y_limits must be a numeric vector of length 2")
    }
  }
  
  if (!error_type %in% c("se", "sd")) {
    stop("Invalid error type: must be 'se' or 'sd'")
  }
  
  if (is.character(threshold)) {
    if (!threshold %in% c("mean", "median")) {
      stop("Invalid threshold: if character, must be 'mean' or 'median'")
    }
  } else if (!is.numeric(threshold)) {
    stop("threshold must be either 'mean', 'median', or numeric")
  }
  
  # Data summarization ----------------------------------------------------------
  summary_df <- data %>%
    group_by(!!x_var) %>%
    summarise(mean = mean(!!y_var, na.rm = TRUE),
              n = n(),
              sd = sd(!!y_var, na.rm = TRUE),
              se = sd / sqrt(n),
              .groups = "drop")
  
  # Determine threshold value --------------------------------------------------
  if (is.character(threshold)) {
    overall <- data[[y_name]]
    if (threshold == "mean") {
      threshold_value <- mean(overall, na.rm = TRUE)
    } else { # threshold == "median"
      threshold_value <- median(overall, na.rm = TRUE)
    }
  } else {
    threshold_value <- threshold
  }
  
  # Choose error column --------------------------------------------------------
  error_col <- error_type  # either "se" or "sd"
  
  # Add fill information if color coding is enabled ----------------------------
  if (color) {
    summary_df <- summary_df %>%
      mutate(fill = if_else(mean >= threshold_value, colors[2], colors[1]))
  }
  
  # Build the plot -------------------------------------------------------------
  p <- ggplot(summary_df, aes(x = factor(!!x_var), y = mean))
  
  if (color) {
    p <- p + geom_bar(stat = "identity", aes(fill = fill), color = "black") +
      scale_fill_identity()
  } else {
    p <- p + geom_bar(stat = "identity", fill = colors[1], color = "black")
  }
  
  p <- p +
    geom_errorbar(aes(ymin = mean - .data[[error_col]], ymax = mean + .data[[error_col]]),
                  width = 0.2) +
    geom_hline(yintercept = threshold_value, linetype = "dashed") +
    labs(title = paste("Average of", y_name, "by each level of", x_name),
         x = x_name, y = y_name) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "none")
  
  # Apply exact y-axis limits if provided (no extra expansion)
  if (!is.null(y_limits)) {
    p <- p + scale_y_continuous(limits = y_limits, expand = c(0, 0))
  }
  
  # Add threshold and error info as a caption so it can be reliably extracted in tests.
  annotation_text <- sprintf("Threshold = %.2f, Error bars represent %s",
                             threshold_value,
                             ifelse(error_type == "se", "standard errors", "standard deviations"))
  p <- p + labs(caption = annotation_text)
  
  return(p)
}

# Load required packages for testing
library(testthat)
library(ggplot2)
library(dplyr)

context("Testing bench_bar function")

test_that("bench_bar returns a ggplot object", {
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = rnorm(20))
  p <- bench_bar(df, group, value)
  expect_s3_class(p, "ggplot")
})

test_that("bench_bar errors when y_var is not numeric", {
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = as.character(rnorm(20)))
  expect_error(bench_bar(df, group, value), "must be numeric")
})

test_that("bench_bar correctly computes threshold using 'mean'", {
  set.seed(123)
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = c(rnorm(10, 5, 1), rnorm(10, 10, 1)))
  p <- bench_bar(df, group, value, threshold = "mean")
  overall_mean <- mean(df$value, na.rm = TRUE)
  # Extract the caption text from the plot
  plot_text <- p$labels$caption
  expect_true(grepl(paste("Threshold =", round(overall_mean, 2)), plot_text))
})

test_that("bench_bar uses numeric threshold correctly", {
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = rnorm(20, 10, 2))
  threshold_val <- 12
  p <- bench_bar(df, group, value, threshold = threshold_val)
  plot_text <- p$labels$caption
  expect_true(grepl(paste("Threshold =", round(threshold_val, 2)), plot_text))
})

test_that("bench_bar errors on invalid error_type", {
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = rnorm(20))
  expect_error(bench_bar(df, group, value, error_type = "invalid"),
               "Invalid error type")
})

test_that("bench_bar errors on invalid threshold string", {
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = rnorm(20))
  expect_error(bench_bar(df, group, value, threshold = "invalid"),
               "Invalid threshold")
})

test_that("bench_bar applies y_limits correctly", {
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = rnorm(20, 5, 1))
  p <- bench_bar(df, group, value, y_limits = c(0, 10))
  # Extract the y-axis limits from the built ggplot object
  gb <- ggplot_build(p)
  y_range <- gb$layout$panel_params[[1]]$y.range
  expect_equal(y_range, c(0, 10))
})

test_that("bench_bar handles color = FALSE correctly", {
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = rnorm(20))
  p <- bench_bar(df, group, value, color = FALSE, colors = c("red", "green"))
  # When color = FALSE, bars should be filled with colors[1] (i.e., "red")
  layer_data <- layer_data(p, 1)
  # Compare the fill value (may be stored as a hex color with full opacity)
  expect_true(all(layer_data$fill == scales::alpha("red", 1) |
                    layer_data$fill == "red"))
})

test_that("bench_bar works with missing values in y_var", {
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = c(rnorm(9, 5, 1), NA, rnorm(10, 7, 1)))
  p <- bench_bar(df, group, value)
  expect_s3_class(p, "ggplot")
})

test_that("bench_bar works with error_type 'sd'", {
  df <- data.frame(group = rep(c("A", "B"), each = 10),
                   value = rnorm(20, 10, 3))
  p_sd <- bench_bar(df, group, value, error_type = "sd")
  expect_s3_class(p_sd, "ggplot")
})
