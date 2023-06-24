#' Benchmark bar plots with threshold line and error bars
#' 
#' Construct a bar plot of the mean of Y for every level of X. User can specify a threshold for a horizontal line used as a benchmark, and choose between standard error and standard deviation error bars.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_var A variable name to be used as the x-axis.
#' @param y_var A variable name to be used as the y-axis.
#' @param color Logical. If TRUE, bars are colored based on their values relative to the threshold value.
#' @param threshold The threshold value for bar coloring. Can be "mean", "median", or a numeric value.
#' @param y_limits A numeric vector of length 2 for specifying y-axis limits.
#' @param colors A character vector of length 2 specifying the colors for bars below and above the threshold.
#' @param error_type A character string specifying the type of error bars to plot. Can be "se" (standard error) or "sd" (standard deviation).
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual geom_errorbar geom_hline labs theme_bw theme element_blank element_text annotate ylim
#' @importFrom dplyr group_by summarize
#' @importFrom assertthat assert_that
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarize}}, \code{\link[assertthat]{assert_that}}
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' library(assertthat)
#'
#' # Example 1: Basic bar plot
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

# Load the packages
library(ggplot2)
library(dplyr)
library(assertthat)

# Save function
bench_bar <- function(data, x_var, y_var, color = TRUE, threshold = "mean", 
                      y_limits = NULL, colors = c("yellow", "blue"), error_type = "se") {
  
  x_var <- deparse(substitute(x_var))
  y_var <- deparse(substitute(y_var))
  
  assert_that(is.numeric(data[[y_var]]), msg = paste(y_var, "is not numeric"))
  
  means <- data %>% 
    group_by(.data[[x_var]]) %>% 
    summarize(mean = mean(.data[[y_var]], na.rm = TRUE),
              se = sd(.data[[y_var]], na.rm = TRUE) / sqrt(n()),
              sd = sd(.data[[y_var]], na.rm = TRUE))
  
  if (threshold == "mean") {
    threshold_value <- mean(data[[y_var]], na.rm = TRUE)
  } else if (threshold == "median") {
    threshold_value <- median(data[[y_var]], na.rm = TRUE)
  } else if (is.numeric(threshold)) {
    threshold_value <- threshold
  } else {
    stop("Invalid threshold value")
  }
  
  if (error_type == "se") {
    error_col <- "se"
    error_text <- "standard errors"
  } else if (error_type == "sd") {
    error_col <- "sd"
    error_text <- "standard deviations"
  } else {
    stop("Invalid error type")
  }
  
  p <- ggplot(data = means, aes(x = as.factor(.data[[x_var]]), y = mean))
  
  if (color) {
    bar_colors <- sapply(means$mean, function(x) if (x >= threshold_value) colors[2] else colors[1])
    p <- p + geom_bar(stat = "identity", position = "dodge", aes(fill = factor(.data[[x_var]]))) +
      scale_fill_manual(values = bar_colors) 
  } else {
    p <- p + geom_bar(stat = "identity", position = "dodge", aes(fill = as.factor(.data[[x_var]]))) 
  }
  
  p <- p +
    geom_errorbar(aes(ymin = mean - .data[[error_col]], ymax = mean + .data[[error_col]]),
                  width = 0.2, position = position_dodge(0.9)) +
    geom_hline(yintercept = threshold_value, linetype = "dashed") +
    labs(title = paste("Average of", y_var, "by each level of", x_var),
         x = x_var, y = y_var) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position = "none") +
    annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
             label = paste("Threshold =", round(threshold_value, 2),
                           ", Error bars represent", error_text),
             size = 3.5, fontface = "italic")
  
  if (!is.null(y_limits)) {
    p <- p + ylim(y_limits)
  }
  
  return(p)
}