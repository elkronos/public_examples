#' Plot profile of selected statistics with error bars
#'
#' This function creates a ggplot profile of selected statistics (mean, median, or sum) with error bars
#' (standard deviation, standard error of the mean, or interquartile range) for the given data frame.
#'
#' @param df A data.frame containing the dataset to be plotted.
#' @param cols A character or numeric vector specifying the columns of the data frame to be plotted.
#'   Default is NULL, which selects all numeric columns.
#' @param title A character string specifying the title of the plot. Default is "Mean +/- 2SD plot of variables".
#' @param xlab A character string specifying the x-axis label. Default is an empty string.
#' @param ylab A character string specifying the y-axis label. Default is NULL, which automatically generates
#'   a label based on the selected statistic and error type.
#' @param line_color A character string specifying the color of the line connecting the points.
#'   Default is "blue".
#' @param point_color A character string specifying the color of the points. Default is "blue".
#' @param error_color A character string specifying the color of the error bars. Default is "black".
#' @param stat A character string specifying the statistic to be plotted. Allowed values are "mean",
#'   "median", and "sum". Default is "mean".
#' @param error_type A character string specifying the error type for the error bars. Allowed values are
#'   "sd" (standard deviation), "sem" (standard error of the mean), and "iqr" (interquartile range).
#'   Default is "sd".
#' @param error_bar A logical value indicating whether to display error bars. Default is TRUE.
#' @param display_stat A logical value indicating whether to display the statistic value above the points.
#'   Default is FALSE.
#' @param error_bar_length A numeric value specifying the length of the error bars in terms of error_type.
#'   Default is 2.
#' @param output_file A character string specifying the file name and format to save the plot. Default is NULL,
#'   which does not save the plot to a file.
#'
#' @return A ggplot object displaying the profile plot with error bars.
#'
#' @import ggplot2
#' @importFrom ggplot2 aes geom_point geom_line labs theme_bw theme element_blank element_text
#'   geom_errorbar geom_hline geom_text ggsave
#'
#' @examples
#' # Example 1: Default parameters
#' plot_profile(synthetic_data)
#'
#' # Example 2: Custom line, point, and error bar colors
#' plot_profile(synthetic_data, line_color = "darkred", point_color = "darkgreen", error_color = "darkblue")
#'
#' # Example 3: Custom title and axis labels
#' plot_profile(synthetic_data, title = "Synthetic Data Example", xlab = "Variables", ylab = "Mean +/- 2SD (Custom Labels)")
#'
#' # Example 4: Using different error types (Standard Error of the Mean)
#' plot_profile(synthetic_data, error_type = "se")
#'
#' # Example 5: Plotting without error bars
#' plot_profile(synthetic_data, error_bar = FALSE)
#'
#' # Example 6: Plotting only specific columns
#' plot_profile(synthetic_data, cols = c("A", "B", "D"))
#'
#' # Example 7: Plot using column positions and specifying ranges
#' plot_profile(synthetic_data, cols = c(1, 3:5))
#'
#' # Example 8: Plot the median instead of the mean
#' plot_profile(synthetic_data, stat = "median")
#'
#' # Example 9: Plot the sum instead of the mean
#' plot_profile(synthetic_data, stat = "sum")
#'
#' # Example 10: Plot with statistic labels
#' plot_profile(synthetic_data, display_stat = TRUE)
#'
#' # Example 11: Plot with different error bar length
#' plot_profile(synthetic_data, error_bar_length = 1)
#'
#' # Example 12: Saving the plot to a file
#' plot_profile(synthetic_data, output_file = "synthetic_data_plot.png")
plot_profile <- function(df, cols = NULL, title = "Mean +/- 2SD plot of variables", xlab = "", ylab = NULL,
                         line_color = "blue", point_color = "blue", error_color = "black",
                         stat = "mean", error_type = "sd", error_bar = TRUE, display_stat = FALSE,
                         error_bar_length = 2, output_file = NULL) {
  
  # Select the columns to be plotted
  if (!is.null(cols)) {
    df <- df[, cols, drop = FALSE]
  }
  
  # Remove non-numeric columns
  df <- df[, sapply(df, is.numeric), drop = FALSE]
  
  # Calculate the central_values and errors
  if (stat == "mean") {
    central_values <- apply(df, 2, mean, na.rm = TRUE)
  } else if (stat == "median") {
    central_values <- apply(df, 2, median, na.rm = TRUE)
  } else if (stat == "sum") {
    central_values <- apply(df, 2, sum, na.rm = TRUE)
  } else {
    stop("Invalid stat value. Allowed values are 'mean', 'median', and 'sum'.")
  }
  
  sds <- apply(df, 2, sd, na.rm = TRUE)
  errors <- sds
  
  # Calculate other error types if specified
  if (error_type == "sem") {
    n <- apply(df, 2, function(x) length(na.omit(x)))
    errors <- sds / sqrt(n)
  } else if (error_type == "iqr") {
    iqrs <- apply(df, 2, IQR, na.rm = TRUE)
    errors <- iqrs / 2
  }
  
  # Set the default y-axis label if not provided
  if (is.null(ylab)) {
    ylab <- paste(stat, "+/-", error_bar_length, error_type, sep = " ")
  }
  
  # Create a data frame for plotting
  plot_data <- data.frame(variable = names(df), central_value = central_values, error = errors)
  
  # Create a ggplot of the means and standard deviations
  p <- ggplot(plot_data, aes(x = variable, y = central_value)) +
    geom_point(size = 3, color = point_color) +
    geom_line(aes(group = 1), color = line_color) +
    labs(x = xlab, y = ylab, title = title) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "none")
  
  # Add error bars if specified
  if (error_bar) {
    p <- p + geom_errorbar(aes(ymin = central_value - error_bar_length * error, ymax = central_value + error_bar_length * error), width = 0.1, color = error_color)
  }
  
  # Display statistic value above the stat itself if specified
  if (display_stat) {
    p <- p + geom_text(aes(label = round(central_value, 2), y = central_value), vjust = -    0.5, hjust = 0.5, size = 3, color = "black")
  }
  
  # Add a horizontal line at y=0 for visual reference
  p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
  
  # Save the plot to a file if specified
  if (!is.null(output_file)) {
    ggsave(output_file, p)
  }
  
  # Return the ggplot object
  return(p)
}


#' Save multiple profile plots as a single image
#'
#' This function creates a grid of profile plots using the arguments provided in arg_list, and saves the resulting grid as a single image.
#'
#' @param arg_list A list of lists containing the arguments for each call to plot_profile.
#' @param nrow An integer specifying the number of rows in the grid. Default is NULL, which calculates the grid dimensions automatically.
#' @param ncol An integer specifying the number of columns in the grid. Default is NULL, which calculates the grid dimensions automatically.
#' @param file_name A character string specifying the file name and format for the saved image. Default is "combined_plots.png".
#' @param width A numeric value specifying the width of the saved image in inches. Default is 15.
#' @param height A numeric value specifying the height of the saved image in inches. Default is 15.
#' @param dpi A numeric value specifying the resolution of the saved image in dots per inch. Default is 300.
#' @param plot_margins A unit object specifying the margins around each plot in the grid. Default is unit(1, "cm").
#' @param labels_size A numeric value specifying the font size of the plot titles. Default is 14.
#'
#' @return NULL. The function saves the combined profile plots to the specified file.
#'
#' @import ggplot2
#' @importFrom ggplot2 ggtitle theme element_text
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid unit
#'
#' @examples
#' # Create a list of plot arguments
#' plot_args_list <- list(list(synthetic_data, title = "Default", stat = "mean"),
#'                        list(synthetic_data, title = "Median", stat = "median"),
#'                        list(synthetic_data, title = "Sum", stat = "sum"),
#'                        list(synthetic_data, title = "Standard Errors", error_type = "sem"),
#'                        list(synthetic_data, title = "Change Bar Length", error_bar_length = 1),
#'                        list(synthetic_data, title = "Change Aesthetic", line_color = "darkred", point_color = "darkgreen", error_color = "darkblue"),
#'                        list(synthetic_data, title = "Remove Error Bars", error_bar = FALSE),
#'                        list(synthetic_data, title = "Pick Columns", cols = c("A", "B", "D")),
#'                        list(synthetic_data, title = "Show Values", display_stat = TRUE))
#'
#' # Save the plots as a single picture
#' save_multi_plot_profile(plot_args_list, file_name = "combined_plots.png", width = 20, height = 20, dpi = 300, plot_margins = unit(2, "cm"), labels_size = 16)
save_multi_plot_profile <- function(arg_list, nrow = NULL, ncol = NULL, file_name = "combined_plots.png",
                                    width = 15, height = 15, dpi = 300, plot_margins = unit(1, "cm"), labels_size = 14) {
  # Check if arg_list is a list
  if (!is.list(arg_list)) {
    stop("arg_list must be a list of lists containing the arguments for each call to plot_profile.")
  }
  
  # Create an empty list to store the plots
  plot_list <- list()
  
  # Create the plots and store them in the list
  for (i in seq_along(arg_list)) {
    args <- arg_list[[i]]
    if (!is.null(args$output_file)) {
      warning("Output file in the arguments list will be ignored.")
      args$output_file <- NULL
    }
    p <- do.call(plot_profile, args)
    p <- p + ggtitle(paste0(i, ". ", p$labels$title)) +
      theme(plot.title = element_text(size = labels_size))
    plot_list[[i]] <- p
  }
  
  # Determine the grid layout if not specified
  if (is.null(nrow) & is.null(ncol)) {
    nrow <- ceiling(sqrt(length(plot_list)))
    ncol <- nrow
  } else if (is.null(nrow)) {
    nrow <- ceiling(length(plot_list) / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(length(plot_list) / nrow)
  }
  
  # Combine the plots in a grid
  combined_plot <- do.call(gridExtra::arrangeGrob, c(plot_list, list(nrow = nrow, ncol = ncol, bottom = plot_margins)))
  
  # Save the combined plot to a file
  ggsave(file_name, combined_plot, width = width, height = height, dpi = dpi)
}