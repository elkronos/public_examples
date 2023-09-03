# Required libraries
library(ggplot2)
library(dplyr)
library(purrr)

# Helper function to abbreviate numbers
abbreviate_number <- function(num) {
  if (is.na(num)) {
    return("NA")
  } else if (num < 1000) {
    return(as.character(round(num, 1)))
  } else if (num >= 1000 & num < 1e6) {
    return(paste0(round(num/1e3, 1), "K"))
  } else if (num >= 1e6 & num < 1e9) {
    return(paste0(round(num/1e6, 1), "M"))
  } else if (num >= 1e9 & num < 1e12) {
    return(paste0(round(num/1e9, 1), "B"))
  } else {
    return(paste0(round(num/1e12, 1), "T"))
  }
}

#' Lollipop Plot with Abbreviated Numbers
#'
#' This function creates a lollipop plot using `ggplot2`. It summarizes the data based on the specified statistic and uses
#' abbreviated number notation (e.g., 1.2K for 1200) for the y-axis and optionally for data labels.
#'
#' @param data A dataframe containing the data to be plotted.
#' @param x_var The variable on the x-axis.
#' @param y_var The variable on the y-axis.
#' @param statistic The type of statistic to be computed for the y-variable. One of 'mean', 'median', or 'sum'.
#'        Default is 'mean'.
#' @param color The color of the lollipops. Default is 'blue'.
#' @param labels Logical indicating whether to show data labels. Default is FALSE.
#' @param threshold An optional numeric threshold to visually distinguish lollipops based on their y-values.
#'        Lollipops below this threshold will have reduced opacity. If provided, a red dotted line will also appear
#'        at this y-value. Default is NULL (no threshold applied).
#' @param label_size Numeric size of data labels. Default is 2.5.
#' @param dot_size Numeric size of the lollipop dots. Default is 10.
#' @param line_width Numeric width of the lollipop lines. Default is 1.2.
#'
#' @return A ggplot2 plot object.
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_point scale_y_continuous theme_minimal labs geom_hline
#' geom_text scale_alpha_identity scale_color_identity
#' @importFrom dplyr group_by summarise mutate
#' @importFrom purrr map_chr
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' fake_data <- data.frame(
#'   category = rep(letters[1:5], each = 1000),
#'   values = c(
#'     runif(1000, 1, 40000),
#'     runif(1000, 1, 40000),
#'     runif(1000, 40001, 60000),
#'     runif(1000, 60001, 10e4),
#'     runif(1000, 60001, 10e4)
#'   )
#' )
#'
#' # Test with 'mean' statistic
#' lollipop_plot(fake_data, category, values, labels = TRUE, threshold = 50000, statistic = "mean")
#' }
lollipop_plot <- function(data, x_var, y_var, statistic = "mean", color = "blue", labels = FALSE, threshold = NULL, label_size = 2.5, dot_size = 10, line_width = 1.2) {
  data <- data %>%
    group_by({{ x_var }}) %>%
    summarise(aggregated = case_when(
      statistic == "mean" ~ mean({{ y_var }}, na.rm = TRUE),
      statistic == "median" ~ median({{ y_var }}, na.rm = TRUE),
      statistic == "sum" ~ sum({{ y_var }}, na.rm = TRUE),
      TRUE ~ mean({{ y_var }}, na.rm = TRUE)
    ))
  
  if (labels) {
    data <- mutate(data, label = map_chr(aggregated, abbreviate_number))
  }
  
  if (!is.null(threshold)) {
    data$alpha <- ifelse(data$aggregated < threshold, 0.2, 1)
  } else {
    data$alpha <- 1
  }
  data$label_color <- ifelse(data$aggregated >= threshold, "white", "black")
  
  p <- ggplot(data, aes(x = factor({{ x_var }}), y = aggregated)) +
    geom_segment(aes(xend = factor({{ x_var }}), yend = 0, alpha = alpha), linetype = "solid", linewidth = line_width, color = color) +
    geom_point(aes(alpha = alpha), color = color, size = dot_size) +
    scale_y_continuous(labels = function(x) map_chr(x, abbreviate_number)) +
    scale_alpha_identity() +
    theme_minimal() +
    labs(y = statistic, x = deparse(substitute(x_var)))
  
  if (!is.null(threshold)) {
    p <- p + geom_hline(yintercept = threshold, linetype = "dotted", color = "red")
  }
  
  if (labels) {
    p <- p + geom_text(aes(label = label, color = label_color), vjust = 0.5, hjust = 0.5, size = label_size) +
      scale_color_identity()
  }
  
  return(p)
}