#' Generate a histogram plot for a numeric variable with optional grouping, density plot, and statistical line
#'
#' @param df Data frame containing the numeric and group variables
#' @param num_var Name of the numeric variable to plot (in quotes)
#' @param group_var Name of the variable to group by (in quotes, default is NULL)
#' @param bin_width Width of the histogram bins (default is NULL, which calculates optimal bin width)
#' @param add_density Logical, whether to add a density plot (default is FALSE)
#' @param title Title for the plot (default is NULL)
#' @param x_label Label for the x-axis (default is NULL, which uses the numeric variable name)
#' @param y_label Label for the y-axis (default is "Count")
#' @param palette Color palette for fill (default is "Set1")
#' @param stat_line Type of statistical line to add to the plot (accepted values are "mean", "median", or use custom_stat_value for a custom value, default is NULL)
#' @param custom_stat_value Custom value for stat_line if desired (default is NULL)
#' @param facet_var Name of the variable to use for faceting (in quotes, default is NULL)
#' @return ggplot object of the generated histogram plot
#' @examples
#' \dontrun{
#' data(mtcars)
#' simple_histo <- histo_group(mtcars, "mpg")
#' print(simple_histo)
#'
#' grouped_histo <- histo_group(mtcars, "mpg", "cyl")
#' print(grouped_histo)
#'
#' density_histo <- histo_group(mtcars, "mpg", "cyl", add_density = TRUE)
#' print(density_histo)
#'
#' mean_line_histo <- histo_group(mtcars, "mpg", "cyl", add_density = TRUE, stat_line = "mean")
#' print(mean_line_histo)
#'
#' faceted_histo <- histo_group(mtcars, "mpg", "cyl", add_density = TRUE, stat_line = "mean", facet_var = "gear")
#' print(faceted_histo)
#' }

# Load package
library(ggplot2)

# Save function
histo_group <- function(df, num_var, group_var = NULL, bin_width = NULL, add_density = FALSE, title = NULL, x_label = NULL, y_label = "Count", palette = "Set1", stat_line = NULL, custom_stat_value = NULL, facet_var = NULL) {
  plot <- ggplot(df, aes(x = !!sym(num_var)))
  
  if (!is.null(group_var)) {
    plot <- plot + aes(fill = factor(!!sym(group_var)))
  }
  
  if (is.null(bin_width)) {
    bin_width <- (max(df[[num_var]], na.rm = TRUE) - min(df[[num_var]], na.rm = TRUE)) / 30
  }
  
  plot <- plot +
    geom_histogram(aes(y = ..count..), alpha = 0.5, position = "identity", binwidth = bin_width) +
    scale_fill_brewer(palette = palette) +
    labs(x = ifelse(is.null(x_label), num_var, x_label), y = y_label, fill = group_var, title = title) +
    theme_minimal()
  
  if (add_density) {
    plot <- plot + geom_density(aes(group = if (!is.null(group_var)) factor(!!sym(group_var)), fill = factor(!!sym(group_var))), alpha = 0.5, adjust = 1)
  }
  
  if (!is.null(stat_line)) {
    if (stat_line == "mean") {
      stat_value <- mean(df[[num_var]], na.rm = TRUE)
    } else if (stat_line == "median") {
      stat_value <- median(df[[num_var]], na.rm = TRUE)
    } else if (!is.null(custom_stat_value)) {
      stat_value <- custom_stat_value
    } else {
      stop("Invalid value for stat_line. Accepted values are 'mean', 'median', or use custom_stat_value for a custom value.")
    }
    
    plot <- plot + geom_vline(aes(xintercept = stat_value), linetype = "dashed", color = "black")
  }
  
  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(as.formula(paste("~", facet_var)))
  }
  
  return(plot)
}